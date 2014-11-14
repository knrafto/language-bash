{-# LANGUAGE LambdaCase, OverloadedStrings, PatternGuards #-}
-- | Shell expansions.
module Language.Bash.Expand
    ( braceExpand
    , TildePrefix(..)
    , tildePrefix
    , splitWord
    ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Traversable
import Text.Parsec.Combinator hiding (optional, manyTill)
import Text.Parsec.Prim       hiding ((<|>), many, token)
import Text.Parsec.String     ()
import Text.PrettyPrint       hiding (char)

import Language.Bash.Pretty
import Language.Bash.Word

-- | A parser over words.
type Parser = Parsec Word ()

-- | Run a 'Parser', failing on a parse error.
parseUnsafe :: String -> Parser a -> Word -> a
parseUnsafe f p w = case parse p (prettyText w) w of
    Left  e -> error $ "Language.Bash.Expand." ++ f ++ ": " ++ show e
    Right a -> a

-- | Parse a general token.
token :: (Span -> Maybe a) -> Parser a
token = tokenPrim (const "") (\pos _ _ -> pos)

-- | Parse an unquoted character satisfying a predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = token $ \case
    Char c | p c -> Just c
    _            -> Nothing

-- | Parse a span that is not an unquoted character satisfying a predicate.
except :: (Char -> Bool) -> Parser Span
except p = token $ \case
    Char c | p c -> Nothing
    s            -> Just s

-- | Parse an unquoted character.
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Parse an unquoted string.
string :: String -> Parser String
string = traverse char

-- | Parse one of the given characters.
oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

-- | Parse anything but a quoted character.
noneOf :: [Char] -> Parser Span
noneOf cs = except (`elem` cs)

-- | Read a number.
readNumber :: MonadPlus m => String -> m Int
readNumber s = case reads (dropPlus s) of
    [(n, "")] -> return n
    _         -> mzero
  where
    dropPlus ('+':t) = t
    dropPlus t       = t

-- | Read a letter.
readAlpha :: MonadPlus m => String -> m Char
readAlpha [c] | isAlpha c = return c
readAlpha _               = mzero

-- | Create a list from a start value, an end value, and an increment.
enum :: (Ord a, Enum a) => a -> a -> Maybe Int -> [a]
enum x y inc = map toEnum [fromEnum x, fromEnum x + step .. fromEnum y]
  where
    step = case inc of
        Nothing | y > x     -> 1
                | otherwise -> 1
        Just i              -> i

-- | Brace expand a word.
braceExpand :: Word -> [Word]
braceExpand = parseUnsafe "braceExpand" (go "")
  where
    go delims = try (brace delims)
            <|> advance delims
            <|> return [""]

    brace delims = do
        _  <- char '{'
        as <- try sequenceExpand <|> braceParts
        _  <- char '}'
        bs <- go delims
        return [ a ++ b | a <- as, b <- bs]

    advance delims = do
        c <- noneOf delims
        map (c :) <$> go delims

    braceParts = concatParts <$> go ",}" `sepBy` char ','

    concatParts []   = ["{}"]
    concatParts [xs] = map (\x -> "{" ++ x ++ "}") xs
    concatParts xss  = concat xss

    sequenceExpand = do
        a   <- sequencePart
        b   <- string ".." *> sequencePart
        c   <- optional (string ".." *> sequencePart)
        inc <- traverse readNumber c
        map fromString <$> (numExpand a b inc <|> charExpand a b inc)
      where
        sequencePart = many1 (satisfy isAlphaNum)

    charExpand a b inc = do
        x <- readAlpha a
        y <- readAlpha b
        return . map (:[]) $ enum x y inc

    numExpand a b inc = do
        x <- readNumber a
        y <- readNumber b
        return . map showPadded $ enum x y inc
      where
        width = max (length a) (length b)

        isPadded ('-':'0':_:_) = True
        isPadded ('0':_:_)     = True
        isPadded _             = False

        showPadded = if isPadded a || isPadded b then pad width else show

        pad w n
            | n < 0     = '-' : pad (w - 1) (negate n)
            | otherwise = replicate (w - length s) '0' ++ s
          where
            s = show n

-- | A tilde prefix.
data TildePrefix
    = Home              -- ^ @~/foo@
    | UserHome String   -- ^ @~fred/foo@
    | PWD               -- ^ @~+/foo@
    | OldPWD            -- ^ @~-/foo@
    | Dirs Int          -- ^ @~N@, @~+N@, @~-N@
    deriving (Eq, Read, Show)

instance Pretty TildePrefix where
    pretty Home         = "~"
    pretty (UserHome s) = "~" <> text s
    pretty PWD          = "~+"
    pretty OldPWD       = "~-"
    pretty (Dirs n)     = "~" <> int n

-- | Strip the tilde prefix of a word, if any.
tildePrefix :: Word -> Maybe (TildePrefix, Word)
tildePrefix w = case parseUnsafe "tildePrefix" split w of
    ('~':s, w') -> Just (readPrefix s, w')
    _           -> Nothing
  where
    split = (,) <$> many (satisfy (/= '/')) <*> getInput

    readPrefix s
        | s == ""                = Home
        | s == "+"               = PWD
        | s == "-"               = OldPWD
        | Just n <- readNumber s = Dirs n
        | otherwise              = UserHome s

-- | Split a word on delimiters.
splitWord :: [Char] -> Word -> [Word]
splitWord ifs = parseUnsafe "splitWord" $ ifsep *> many (word <* ifsep)
  where
    ifsep = many  (oneOf  ifs)
    word  = many1 (noneOf ifs)
