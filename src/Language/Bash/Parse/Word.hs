{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
-- | Word parsers.
module Language.Bash.Parse.Word
    ( word
    , heredoc
    , name
    , subscript
    , arith
    ) where

import           Prelude                     hiding (span)

import           Control.Applicative
import           Data.Maybe
import           Text.Parsec                 hiding ((<|>), optional, many)

import           Language.Bash.Parse.Builder ((<+>))
import qualified Language.Bash.Parse.Builder as B
import           Language.Bash.Pretty
import           Language.Bash.Word

-- | Parse a matched pair.
matchedPair
    :: Stream s m Char
    => Char
    -> Char
    -> ParsecT s u m (Maybe Span)
    -> ParsecT s u m Word
matchedPair begin end esc =
    catMaybes <$ char begin <*> many inner <* char end
  where
    inner = esc
        <|> Just . Char <$> satisfy (/= end)

-- | Parse a backslash-escaped sequence.
escape :: Stream s m Char => ParsecT s u m (Maybe Span)
escape = char '\\' *> rest
  where
    rest = Nothing       <$  char '\n'
       <|> Just . Escape <$> anyChar

-- | Parse a single-quoted string.
singleQuote :: Stream s m Char => ParsecT s u m Span
singleQuote = Single <$> matchedPair '\'' '\'' empty

-- | Parse a double-quoted string.
doubleQuote :: Stream s m Char => ParsecT s u m Span
doubleQuote = Double <$> matchedPair '"' '"' inner
  where
    inner = escape
        <|> Just <$> backquote
        <|> Just <$> dollar

-- | Parse an ANSI C string.
ansiQuote :: Stream s m Char => ParsecT s u m Span
ansiQuote = undefined

-- | Parse a locale string.
localeQuote :: Stream s m Char => ParsecT s u m Span
localeQuote = Locale <$> matchedPair '"' '"' inner
  where
    inner = escape
        <|> Just <$> backquote
        <|> Just <$> dollar

-- | Parse a special quote.
specialQuote :: Stream s m Char => ParsecT s u m Span
specialQuote = char '$' *> (ansiQuote <|> localeQuote)

-- | Parse a backquoted string.
backquote :: Stream s m Char => ParsecT s u m Span
backquote = undefined

-- | Parse a parenthesized substitution.
subst :: Stream s m Char => ParsecT s u m String
subst = undefined

-- | Parse a parameter, arithmetic, or command substitution.
dollar :: Stream s m Char => ParsecT s u m Span
dollar = char '$' *> rest
  where
    rest = try arithSubst
       <|> try commandSubst
       <|> parameterSubst
       <|> pure (Char '$')

    parameterSubst = ParameterSubst <$> inner
      where
        inner = try braceSubst
            <|> badSubst
            <|> bareSubst

    specialParam = (:[]) <$> oneOf "$*@?-!_"

    braceSubst = do
        _ <- char '{'
        inverted <- isJust <$> optional (char '!')
        _ <- char '}'

    bareSubst = do
        parameter <- Parameter <$> bareParam <*> pure Nothing
        return Bare{..}
      where
        bareParam = name
                <|> (:[]) <$> digit
                <|> specialParam

    badSubst = do
        s <- prettyText <$> matchedPair '{' '}' inner
        return $ BadSubst ("${" ++ s ++ "}")
      where
        inner = escape
            <|> Just <$> singleQuote
            <|> Just <$> doubleQuote
            <|> Just <$> backquote
            <|> Just <$> dollar

    arithSubst = ArithSubst <$ string "((" <*> arith <* string "))"

    commandSubst = CommandSubst <$> subst

-- | Parse a process substitution.
processSubst :: Stream s m Char => ParsecT s u m Span
processSubst = ProcessSubst <$> processSubstOp <*> subst
  where
    processSubstOp = In  <$ char '<'
                 <|> Out <$ char '>'

-- | Parse a word part.
span :: Stream s m Char => ParsecT s u m (Maybe Span)
span = try escape
   <|> Just <$> singleQuote
   <|> Just <$> doubleQuote
   <|> Just <$> backquote
   <|> Just <$> try specialQuote
   <|> Just <$> dollar
   <|> Just <$> try processSubst

-- | Parse a word.
word :: Stream s m Char => ParsecT s u m Word
word = catMaybes <$> many1 wordPart <?> "word"
  where
    wordPart = span
           <|> Just . Char <$> noneOf " \t\n&|;()<>"

-- | Parse a here document.
heredoc :: Stream s m Char => ParsecT s u m Word
heredoc = catMaybes <$> many heredocPart <?> "here document"
  where
    heredocPart = try escape
              <|> Just <$> dollar
              <|> Just . Char <$> anyChar

-- | Parse a parameter name.
name :: Stream s m Char => ParsecT s u m String
name = (:) <$> nameStart <*> many nameLetter
  where
    nameStart  = letter   <|> char '_'
    nameLetter = alphaNum <|> char '_'

-- | Parse a subscript.
subscript :: Stream s m Char => ParsecT s u m Word
subscript = matchedPair '[' ']' span <?> "subscript"

-- | Parse an arithmetic expression.
arith :: Stream s m Char => ParsecT s u m String
arith = B.toString <$> parens <?> "arithmetic expression"
  where
    parens = B.many inner
    inner  = B.matchedPair '(' ')' parens
