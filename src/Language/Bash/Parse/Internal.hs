{-# LANGUAGE FlexibleContexts #-}
-- | Low-level parsers.
module Language.Bash.Parse.Internal
    ( skipSpace
    , word
    , arith
    , name
    , assign
    , operator
    , unquote
    ) where

import           Control.Applicative
import           Data.Monoid
import           Text.Parsec.Char
import           Text.Parsec.Combinator      hiding (optional)
import           Text.Parsec.Prim            hiding ((<|>), many)
import           Text.Parsec.String          ()

import           Language.Bash.Parse.Builder (Builder, (<+>))
import qualified Language.Bash.Parse.Builder as B
import           Language.Bash.Syntax

-- | @surroundBy p sep@ parses zero or more occurences of @p@, beginning,
-- ending, and separated by @sep@.
surroundBy
    :: Stream s m t
    => ParsecT s u m a
    -> ParsecT s u m sep
    -> ParsecT s u m [a]
surroundBy p sep = sep *> endBy p sep

-- | Skip spaces, tabs, and comments.
skipSpace :: Stream s m Char => ParsecT s u m ()
skipSpace = skipMany spaceChar <* optional comment <?> "whitespace"
  where
    spaceChar = try (B.string "\\\n")
            <|> B.oneOf " \t"

    comment = char '#' *> many (satisfy (/= '\n'))

-- | Parse a backslash-escaped sequence.
escape :: Stream s m Char => ParsecT s u m Builder
escape = B.char '\\' <+> B.anyChar

-- | Parse a single-quoted string.
singleQuote :: Stream s m Char => ParsecT s u m Builder
singleQuote = B.matchedPair '\'' '\'' empty

-- | Parse a double-quoted string.
doubleQuote :: Stream s m Char => ParsecT s u m Builder
doubleQuote = B.matchedPair '"' '"' $ escape <|> backquote <|> dollar

-- | Parse an ANSI C string.
ansiQuote :: Stream s m Char => ParsecT s u m Builder
ansiQuote = B.char '$' <+> B.matchedPair '\'' '\'' escape

-- | Parse a locale string.
localeQuote :: Stream s m Char => ParsecT s u m Builder
localeQuote = B.char '$' <+> doubleQuote

-- | Parse a backquoted string.
backquote :: Stream s m Char => ParsecT s u m Builder
backquote = B.matchedPair '`' '`' escape

-- | Parse a brace-style parameter expansion, an arithmetic substitution,
-- or a command substitution.
dollar :: Stream s m Char => ParsecT s u m Builder
dollar = B.char '$' <+> rest
  where
    rest = braceParameter
       <|> try arithSubst
       <|> commandSubst
       <|> return mempty

    braceParameter = B.matchedPair '{' '}' $
            escape
        <|> singleQuote
        <|> doubleQuote
        <|> backquote
        <|> dollar

    arithSubst = B.string "((" <+> parens <+> B.string "))"

    commandSubst = subst

-- | Parse a process substitution.
processSubst :: Stream s m Char => ParsecT s u m Builder
processSubst = B.oneOf "<>" <+> subst

-- | Parse a parenthesized substitution.
subst :: Stream s m Char => ParsecT s u m Builder
subst = B.matchedPair '(' ')' $
        subst
    <|> B.char '#' <+> B.many (B.satisfy (/= '\n')) <+> B.char '\n'
    <|> escape
    <|> singleQuote
    <|> doubleQuote
    <|> backquote
    <|> dollar

-- | Parse a parenthesized expression.
parens :: Stream s m Char => ParsecT s u m Builder
parens = B.many inner
  where
    inner = B.matchedPair '(' ')' parens

-- | Parse a word part.
wordSpan :: Stream s m Char => ParsecT s u m Builder
wordSpan = mempty <$ try (string "\\\n")
       <|> escape
       <|> singleQuote
       <|> doubleQuote
       <|> try ansiQuote
       <|> try localeQuote
       <|> backquote
       <|> dollar
       <|> try processSubst

-- | Parse a word.
word :: Stream s m Char => ParsecT s u m String
word = B.toString <$> B.many wordPart <?> "word"
  where
    wordPart = wordSpan
           <|> B.noneOf " \t\n|&;()<>"

-- | Parse an arithmetic expression.
arith :: Stream s m Char => ParsecT s u m String
arith = B.toString <$> parens <?> "arithmetic expression"

-- | Parse a name.
name :: Stream s m Char => ParsecT s u m String
name = (:) <$> nameStart <*> many nameLetter
  where
    nameStart  = letter   <|> char '_'
    nameLetter = alphaNum <|> char '_'

-- | Parse an assignment.
assign :: Stream s m Char => ParsecT s u m Assign
assign = Assign <$> lvalue <*> assignOp <*> rvalue <?> "assignment"
  where
    lvalue = LValue <$> name <*> optional subscript

    subscript = B.toString <$> B.span '[' ']' wordSpan

    assignOp = Equals     <$ string "="
           <|> PlusEquals <$ string "+="

    rvalue = RArray <$  char '(' <*> arrayElems <* char ')'
         <|> RValue <$> word

    arrayElems = arrayElem `surroundBy` skipArraySpace

    arrayElem = do
        s <- optional (subscript <* char '=')
        w <- word
        case (s, w) of
            (Nothing, "") -> empty
            _             -> return (s, w)

    skipArraySpace = char '\n' `surroundBy` skipSpace

-- | Parse the longest available operator from a list.
operator :: Stream s m Char => [String] -> ParsecT s u m String
operator ops = go ops <?> "operator"
  where
    go xs
        | null xs      = empty
        | "" `elem` xs = try (continue xs) <|> pure ""
        | otherwise    = continue xs

    continue xs = do
        c <- anyChar
        (c :) <$> go (prefix c xs)

    prefix c = map tail . filter (\x -> not (null x) && head x == c)

-- | Unquote a word.
unquote :: String -> String
unquote s = case parse unquoteBare s s of
    Left _   -> s
    Right s' -> B.toString s'
  where
    unquoteBare = B.many $
            try unquoteEscape
        <|> try unquoteSingle
        <|> try unquoteDouble
        <|> try unquoteAnsi
        <|> try unquoteLocale
        <|> B.anyChar

    unquoteEscape = char '\\' *> B.anyChar
    unquoteSingle = B.span '\'' '\'' empty
    unquoteDouble = B.span '\"' '\"' unquoteEscape
    unquoteAnsi   = char '$' *> B.span '\'' '\'' unquoteEscape
    unquoteLocale = char '$' *> unquoteDouble
