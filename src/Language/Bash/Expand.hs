{-# LANGUAGE PatternGuards #-}
-- | Bash expansions.
module Language.Bash.Expand
    ( braceExpand
    , TildePrefix(..)
    , tildePrefix
    , splitWord
    , unquote
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Monoid
import           Data.Traversable
import           Text.Parsec                  hiding ((<|>), optional)

import qualified Language.Bash.Parse.Builder  as B
import           Language.Bash.Parse.Internal
import           Language.Bash.Syntax

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

-- | Parse a word with a parser that should always succeed.
parseWord :: String -> Parsec String () a -> String -> a
parseWord fname p s = case parse p s s of
    Left  e -> error $ "Language.Bash.Expand." ++ fname ++ ": " ++ show e
    Right r -> r

-- | Pad a number to a specified width
showPadded :: Int -> Int -> String
showPadded w n
    | n < 0     = '-' : showPadded (w - 1) (negate n)
    | otherwise = replicate (w - length s) '0' ++ s
  where
    s = show n

-- | Read a number.
fromNumber :: MonadPlus m => String -> m Int
fromNumber s = case s of
    '+':s' -> readNumber s'
    _      -> readNumber s
  where
    readNumber t = case reads t of
        [(n,"")] -> return n
        _        -> mzero

-- | Read a character.
fromAlpha :: MonadPlus m => String -> m Char
fromAlpha [c] | isAlpha c = return c
fromAlpha _               = mzero

-- | Create a list from a start value, an end value, and an increment.
enum :: (Ord a, Enum a) => a -> a -> Maybe Int -> [a]
enum x y inc = map toEnum [fromEnum x, fromEnum x + step .. fromEnum y]
  where
    step = case inc of
        Nothing | y > x     -> 1
                | otherwise -> 1
        Just i              -> i

-- | Brace expand a word, including sequences.
braceExpand :: Word -> [Word]
braceExpand = parseWord "braceExpand" (map B.toString <$> brace "")
  where
    brace delims = try (expansion delims)
               <|> return <$> gobble delims

    expansion delims = do
        a  <- gobble ('{':delims)
        _  <- char '{'
        bs <- try sequenceExpansion <|> braceParts
        _  <- char '}'
        cs <- brace delims
        return [a <> b <> c | b <- bs, c <- cs]

    braceParts = concatParts <$> brace ",}" `sepBy` char ','

    concatParts []   = [B.fromString "{}"]
    concatParts [xs] = map (\x -> B.fromChar '{' <> x <> B.fromChar '}') xs
    concatParts xss  = concat xss
 
    sequenceExpansion = do
        a   <- sequencePart
        b   <- string ".." *> sequencePart
        c   <- optional (string ".." *> sequencePart)
        inc <- traverse fromNumber c
        charExpansion a b inc <|> numberExpansion a b inc
      where
        sequencePart = many1 (satisfy (\c -> isAlphaNum c || c == '-'))
 
    charExpansion a b inc = do
        x <- fromAlpha a
        y <- fromAlpha b
        return . map B.fromChar $ enum x y inc
 
    numberExpansion a b inc = do
        x <- fromNumber a
        y <- fromNumber b
        return . map (B.fromString . render) $ enum x y inc
      where
        width = max (length a) (length b)

        isPadded ('-':'0':_:_) = True
        isPadded ('0':_:_)     = True
        isPadded _             = False
 
        render = if isPadded a || isPadded b
                 then showPadded width
                 else show

-- | A Bash tilde prefix of a word.
data TildePrefix
    = Home             -- ^ '~/foo'
    | UserHome String  -- ^ '~fred/foo'
    | PWD              -- ^ '~+/foo'
    | OldPWD           -- ^ '~-/foo'
    | Dirs Int         -- ^ '~N', '~+N', '~-N'
    deriving (Eq, Read, Show)

-- | Get the tilde prefix of a word, if there is one. The remaining word
-- may be empty.
tildePrefix :: Word -> (Maybe TildePrefix, Word)
tildePrefix w = case parse tilde "" w of
    Left _        -> (Nothing, w)
    Right (p, w') -> (Just p, w')
  where
    tilde = do
        _ <- char '~'
        s <- B.toString <$> gobble "/"
        r <- getInput
        return (readTilde s, r)

    readTilde s
        | s == ""                = Home
        | s == "+"               = PWD
        | s == "-"               = OldPWD
        | Just n <- fromNumber s = Dirs n
        | otherwise              = UserHome s

-- | Split a word into parts based on a the specified delimiters.
splitWord :: [Char] -> Word -> [Word]
splitWord ifs = parseWord "splitWord" (map B.toString <$> go)
  where
    go = do
        skipMany (oneOf ifs)
        [] <$ eof <|> (:) <$> gobble ifs <*> go

-- | Unquote a word.
unquote :: Word -> String
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
    unquoteAnsi   = char '$' *> B.span '\'' '\'' (try ansiEscape)
    unquoteLocale = char '$' *> unquoteDouble

    ansiEscape = B.fromChar <$ char '\\' <*> escapeCode

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
