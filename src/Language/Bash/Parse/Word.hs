{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
-- | Word parsers.
module Language.Bash.Parse.Word
    ( word
    , heredocWord
    , name
    , subscript
    , arith
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Text.Parsec                 hiding ((<|>), optional, many)

import           Language.Bash.Parse.Builder ((<+>))
import qualified Language.Bash.Parse.Builder as B
import           Language.Bash.Pretty
import           Language.Bash.Word

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

-- | Parse a span until a delimeter.
spans
    :: Stream s m Char
    => [Char]              -- ^ Delimiters
    -> Bool                -- ^ Remove escaped newlines
    -> ParsecT s u m Span  -- ^ Inner spans
    -> ParsecT s u m Word
spans delims removeEscapedNewline innerSpan = catMaybes <$> many inner
  where
    inner = Nothing     <$  escapedNewline
        <|> Just        <$> innerSpan
        <|> Just . Char <$> noneOf delims

    escapedNewline = if removeEscapedNewline
                     then try (string "\\\n")
                     else empty

-- | Parse a matched pair.
matchedPair
    :: Stream s m Char
    => Char                -- ^ Start character
    -> Char                -- ^ End character
    -> Bool                -- ^ Remove escaped newlines
    -> ParsecT s u m Span  -- ^ Inner spans
    -> ParsecT s u m Word
matchedPair begin end removeEscapedNewline innerSpan =
    char begin *> spans [end] removeEscapedNewline innerSpan <* char end

-- | Parse a single-quoted string.
singleQuote :: Stream s m Char => ParsecT s u m Span
singleQuote = Single <$> matchedPair '\'' '\'' False empty

-- | Parse a double-quoted string.
doubleQuote :: Stream s m Char => ParsecT s u m Span
doubleQuote = Double <$> matchedPair '"' '"' True inner
  where
    inner = Escape <$ char '\\' <*> oneOf "$\\`\""
        <|> backquote
        <|> dollar

-- | Parse an ANSI C string.
ansiQuote :: Stream s m Char => ParsecT s u m Span
ansiQuote = ANSIC <$ char '$' <*> matchedPair '\'' '\'' True escape
  where
    escape = fromChar <$ char '\\' <*> escapeCode

    fromChar c | c `elem` "\\\'" = Escape c
               | otherwise       = Char c

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

    codes chars replacements = try $ do
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

-- | Parse a locale string.
localeQuote :: Stream s m Char => ParsecT s u m Span
localeQuote = Locale <$ char '$' <*> matchedPair '"' '"' True inner
  where
    inner = try (Escape <$ char '\\' <*> oneOf "$\\`\"")
        <|> backquote
        <|> dollar

-- | Parse a backquoted string.
backquote :: Stream s m Char => ParsecT s u m Span
backquote = Backquote <$> matchedPair '`' '`' False escape
  where
    escape = try (Escape <$ char '\\' <*> oneOf "$\\`")

-- | Parse a dollar substitution.
dollar :: Stream s m Char => ParsecT s u m Span
dollar = char '$' *> rest
  where
    rest = arithSubst
       <|> commandSubst
       <|> braceSubst
       <|> bareSubst
       <|> pure (Char '$')

    arithSubst   = ArithSubst   <$  try (string "((") <*> arith <* string "))"
    commandSubst = CommandSubst <$> subst
    braceSubst   = ParamSubst   <$  char '{' <*> paramSubst <* char '}'

    bareSubst = ParamSubst . Bare <$> bareParam
      where
        bareParam = Parameter <$> bareParamName <*> pure Nothing

        bareParamName = name
                    <|> specialName
                    <|> (:[]) <$> digit

-- | Parse a process substitution.
processSubst :: Stream s m Char => ParsecT s u m Span
processSubst = ProcessSubst <$> processSubstOp <*> subst
  where
    processSubstOp = In  <$ char '<'
                 <|> Out <$ char '>'

-- | Parse a parenthesized substitution.
subst :: Stream s m Char => ParsecT s u m String
subst = B.toString <$ char '(' <*> B.many parens <* char ')'
  where
    parens = B.char '(' <+> B.many parens <+> B.char ')'
         <|> B.char '#' <+> B.many (B.satisfy (/= '\n')) <+> B.char '\n'
         <|> B.char '\\' <+> B.anyChar
         <|> fromSpan <$> singleQuote
         <|> fromSpan <$> doubleQuote
         <|> fromSpan <$> backquote
         <|> fromSpan <$> dollar
         <|> B.satisfy (/= ')')

    fromSpan = B.fromString . prettyText

-- | Parse a parameter substitution.
paramSubst :: Stream s m Char => ParsecT s u m ParamSubst
paramSubst = try prefixSubst
         <|> try indicesSubst
         <|> try lengthSubst
         <|> normalSubst
  where
    param = Parameter <$> name        <*> optional subscript
        <|> Parameter <$> many1 digit <*> pure Nothing
        <|> Parameter <$> specialName <*> pure Nothing

    switch c = isJust <$> optional (char c)

    doubled p = do
        a <- p
        d <- fmap isJust . optional . try $ do
            b <- p
            guard (a == b)
        return (a, d)

    direction = Front <$ char '#'
            <|> Back  <$ char '%'

    substWord delims = spans delims True inner
      where
        inner = Escape <$ char '\\' <*> anyChar
            <|> singleQuote
            <|> doubleQuote
            <|> backquote
            <|> dollar

    prefixSubst = do
        _        <- char '!'
        prefix   <- name
        modifier <- oneOf "*@"
        return Prefix{..}

    indicesSubst = do
        _   <- char '!'
        n   <- name
        _   <- char '['
        sub <- oneOf "*@"
        _   <- char ']'
        let parameter = Parameter n (Just [Char sub])
        return Indices{..}

    lengthSubst = do
        _         <- char '#'
        parameter <- param
        return Length {..}

    normalSubst = do
        indirect <- switch '!'
        parameter <- param
        choice . map try $
            [ do testNull <- switch ':'
                 altOp <- AltDefault <$ char '-'
                      <|> AltAssign  <$ char '='
                      <|> AltError   <$ char '?'
                      <|> AltReplace <$ char '+'
                 altWord <- substWord "}"
                 return Alt{..}
            , do subOffset <- char ':' *> substWord ":}"
                 subLength <- option [] (char ':' *> substWord "}")
                 return Substring{..}
            , do (deleteDirection, longest) <- doubled direction
                 pattern <- substWord "}"
                 return Delete{..}
            , do _ <- char '/'
                 replaceAll <- switch '/'
                 replaceDirection <- optional direction
                 pattern <- substWord "/}"
                 replacement <- option [] (char '/' *> substWord "}")
                 return Replace{..}
            , do (letterCaseOp, convertAll) <- doubled $
                        ToLower <$ char ','
                    <|> ToUpper <$ char '^'
                 pattern <- substWord "}"
                 return LetterCase{..}
            , return Brace {..}
            ]

-- | Parse any span that may occur in a word.
wordSpan :: Stream s m Char => ParsecT s u m Span
wordSpan = try (Escape <$ char '\\' <*> anyChar)
       <|> singleQuote
       <|> doubleQuote
       <|> try ansiQuote
       <|> try localeQuote
       <|> backquote
       <|> dollar
       <|> try processSubst

-- | Parse a word.
word :: Stream s m Char => ParsecT s u m Word
word = spans " \t\n$|;()<>" True wordSpan

-- | Parse a here document.
heredocWord :: Stream s m Char => ParsecT s u m Word
heredocWord = spans [] True inner
  where
    inner = try (Escape <$ char '\\' <*> oneOf "$\\`")
        <|> backquote
        <|> dollar

-- | Parse a parameter name.
name :: Stream s m Char => ParsecT s u m String
name = (:) <$> nameStart <*> many nameLetter
  where
    nameStart  = letter   <|> char '_'
    nameLetter = alphaNum <|> char '_'

-- | Parse a special parameter name.
specialName :: Stream s m Char => ParsecT s u m String
specialName = (:[]) <$> oneOf "*@#?-$!_"

-- | Parse a subscript.
subscript :: Stream s m Char => ParsecT s u m Word
subscript = matchedPair '[' ']' True wordSpan

-- | Parse an arithmetic expression.
arith :: Stream s m Char => ParsecT s u m String
arith = B.toString <$> parens <?> "arithmetic expression"
  where
    parens = B.many inner
    inner  = B.matchedPair '(' ')' parens
