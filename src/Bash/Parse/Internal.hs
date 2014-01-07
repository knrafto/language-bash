{-# LANGUAGE FlexibleContexts, LambdaCase #-}
-- | Low-level parsers.
module Bash.Parse.Internal
    ( skipSpace
    , word
    , word1
    , operator
    , assign
    , arith
    ) where

import Prelude                hiding (span)

import Control.Applicative
import Data.Char
import Data.List              hiding (span)
import Text.Parsec.Char
import Text.Parsec.Combinator hiding (optional)
import Text.Parsec.Prim       hiding ((<|>), many)

import Bash.Word
import Bash.Types

-- | @upTo n p@ parses zero to @n@ occurences of @p@.
upTo :: Alternative f => Int -> f a -> f [a]
upTo m p = go m
  where
    go n | n < 0     = empty
         | n == 0    = pure []
         | otherwise = (:) <$> p <*> go (n - 1) <|> pure []

-- | @upTo1 n p@ parses one to @n@ occurences of @p@.
upTo1 :: Alternative f => Int -> f a -> f [a]
upTo1 n p = (:) <$> p <*> upTo (n - 1) p

-- | @surroundBy p sep@ parses zero or more occurences of @p@, beginning,
-- ending, and separated by @sep@.
surroundBy
    :: Stream s m t
    => ParsecT s u m a
    -> ParsecT s u m sep
    -> ParsecT s u m [a]
surroundBy p sep = sep *> endBy p sep

-- | Parse a comment
comment :: Stream s m Char => ParsecT s u m String
comment = char '#' *> many (satisfy (/= '\n'))

-- | Skip spaces, tabs, and comments.
skipSpace :: Stream s m Char => ParsecT s u m ()
skipSpace = skipMany spaceChar <* optional comment
  where
    spaceChar = () <$ try (string "\\\n")
            <|> () <$ oneOf " \t"

-- | Parse a name.
name :: Stream s m Char => ParsecT s u m String
name = (:) <$> nameStart <*> many nameLetter
  where
    nameStart  = letter   <|> char '_'
    nameLetter = alphaNum <|> char '_'

-- | A helper type for parsing parenthesized strings.
data Parens
    = Span Span
    | Parens [Parens]
    | Comment String

-- | Convert 'Parens' back to a string.
fromParens :: [Parens] -> String
fromParens = ($ "") . showParens
  where
    showMany f = foldr (.) id . map f
    showParens = showMany $ \case
        Span s    -> showSpan s
        Parens ps -> showChar '(' . showParens ps . showChar ')'
        Comment s -> showChar '#' . showString s

-- | @span start end escape@ parses a span starting with @start@ and ending
-- with @end@, with possible @escape@ sequences inside.
span
    :: Stream s m Char
    => Char -> Char
    -> ParsecT s u m Span
    -> ParsecT s u m Word
span start end escape = Word <$ char start <*> many inner <* char end
  where
    inner = escape
        <|> Char <$> satisfy (/= end)

-- | Parse an ANSI C string in single quotes.
ansiString :: Stream s m Char => ParsecT s u m Word
ansiString = span '\'' '\'' (try escape)
  where
    escape = Escape <$ char '\\' <*> escapeCode

    escapeCode = charCodes
             <|> char 'x' *> hex 2
             <|> char 'u' *> hex 4
             <|> char 'U' *> hex 8
             <|> oct 3
             <|> char 'c' *> ctrlCodes

    charCodes = codes "abeEfnrtv\\\'\"" "\a\b\ESC\ESC\f\n\r\t\v\\\'\""

    ctrlCodes = '\FS' <$ try (string "\\\\")
            <|> codes "@ABCDEFGHIJKLMOPQRSTUVWXYZ[]^_?"
                      ("\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\BS\HT\LF\VT\FF" ++
                       "\CR\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM" ++
                       "\SUB\ESC\GS\RS\US\DEL")

    codes chars replacements = do
        c <- anyChar
        case lookup c table of
            Nothing -> unexpected [c]
            Just c' -> return c'
      where
        table = zip chars replacements

    oct n = number n 8 octDigit
    hex n = number n 16 hexDigit

    number maxDigits base baseDigit = do
        digits <- map digitToInt <$> upTo1 maxDigits baseDigit
        let n = foldl' (\x d -> base*x + d) 0 digits
        return $ if n > ord maxBound then '\0' else chr n  -- arbitrary

-- | Parse a word part.
wordSpan :: Stream s m Char => ParsecT s u m Span
wordSpan = try (string "\\\n" *> wordPart)
       <|> escape
       <|> single
       <|> double
       <|> backquote
       <|> try specialQuote
       <|> dollar
       <|> try processSubst
  where
    escape    = Escape <$ char '\\' <*> anyChar
    single    = Single <$> span '\'' '\'' empty
    double    = Double <$> span '\"' '\"' (escape <|> backquote <|> dollar)
    backquote = Backquote . toString <$> span '`' '`' escape

    specialQuote = char '$' *> rest
      where
        rest = Single <$> ansiString
           <|> double

    dollar = char '$' *> rest
      where
        rest = parameter
           <|> braceParameter
           <|> try arithSubst
           <|> commandSubst
           <|> return (Char '$')

    parameter = Parameter <$> rest
      where
        rest = name
           <|> return <$> digit
           <|> return <$> oneOf "*@#?-$!_"

    braceParameter = BraceParameter <$> span '{' '}' inner
      where
        inner = escape
            <|> single
            <|> double
            <|> backquote
            <|> dollar

    arithSubst   = ArithSubst <$ string "((" <*> arith <* string "))"
    commandSubst = CommandSubst <$> subst
    processSubst = ProcessSubst <$> oneOf "<>" <*> subst

    subst = fromParens <$> parens
      where
        parens = char '(' *> many inner <* char ')'

        inner = Parens <$> parens
            <|> Comment <$> comment
            <|> Span <$> innerSpan

        innerSpan = escape
                <|> single
                <|> double
                <|> backquote
                <|> dollar
                <|> Char <$> satisfy (/= ')')

-- | Parse a part of a normal word.
wordPart :: Stream s m Char => ParsecT s u m Span
wordPart = wordSpan
       <|> Char <$> noneOf " \t\n|&;()<>"

-- | Parse a word.
word :: Stream s m Char => ParsecT s u m Word
word = Word <$> many wordPart

-- | Parse a nonempty word.
word1 :: Stream s m Char => ParsecT s u m Word
word1 = Word <$> many1 wordPart

-- | Lex a token in assignment mode. This lexes only assignment statements.
assign :: Stream s m Char => ParsecT s u m Assign
assign = Assign <$> lvalue <*> assignOp <*> rvalue
  where
    lvalue = LValue <$> name <*> optional subscript

    subscript = span '[' ']' wordSpan

    assignOp = Equals     <$ string "="
           <|> PlusEquals <$ string "+="

    rvalue = RArray <$  char '(' <*> arrayElems <* char ')'
         <|> RValue <$> word

    arrayElems = arrayElem `surroundBy` skipArraySpace

    arrayElem = (,) <$> (Just <$> subscript) <* char '=' <*> word
            <|> (,) <$> pure Nothing <*> word1

    skipArraySpace = char '\n' `surroundBy` skipSpace

-- | Parse the longest available operator from a list.
operator :: Stream s m Char => [String] -> ParsecT s u m String
operator = go
  where
    go ops
        | null ops      = empty
        | "" `elem` ops = try (continue ops) <|> pure ""
        | otherwise     = continue ops

    continue ops = do
        c <- anyChar
        (c :) <$> go (prefix c ops)

    prefix c = map tail . filter (\x -> not (null x) && head x == c)

-- | Parse an arithmetic expression.
arith :: Stream s m Char => ParsecT s u m String
arith = fromParens <$> go
  where
    go = many paren

    paren = Parens <$ char '(' <*> go <* char ')'
        <|> Span . Char <$> satisfy (/= ')')
