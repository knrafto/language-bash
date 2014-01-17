{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- | Shell expansions.
module Language.Bash.Expand
    ( braceExpand
    ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Traversable
import Text.Parsec.Combinator hiding (optional, manyTill)
import Text.Parsec.Prim       hiding ((<|>), many, token)
import Text.Parsec.String     ()

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

-- | Parse a word until an unquoted character that satisfies the predicate
-- appears.
manyTill :: (Char -> Bool) -> Parser Word
manyTill p = many $ token $ \case
    Char c | p c -> Nothing
    s            -> Just s

-- | Parse an unquoted character.
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Parse an unquoted string.
string :: String -> Parser String
string = traverse char

-- | Read a number.
fromNumber :: MonadPlus m => String -> m Int
fromNumber s = case s of
    '+':s' -> readNumber s'
    _      -> readNumber s
  where
    readNumber t = case reads t of
        [(n, "")] -> return n
        _         -> mzero

-- | Read a letter.
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

-- | Brace expand a word.
braceExpand :: Word -> [Word]
braceExpand = parseUnsafe "braceExpand" (go "")
  where
    go delims = try (brace delims)
            <|> (:[]) <$> manyTill (`elem` delims)

    brace delims = do
        a  <- manyTill (`elem` ('{' : delims))
        _  <- char '{'
        bs <- try sequenceExpand <|> braceParts
        _  <- char '}'
        cs <- go delims
        return [ a ++ b ++ c | b <- bs, c <- cs ]

    braceParts = concatParts <$> go ",}" `sepBy` char ','

    concatParts []   = ["{}"]
    concatParts [xs] = map (\x -> "{" ++ x ++ "}") xs
    concatParts xss  = concat xss

    sequenceExpand = do
        a   <- sequencePart
        b   <- string ".." *> sequencePart
        c   <- optional (string ".." *> sequencePart)
        inc <- traverse fromNumber c
        map fromString <$> (numExpand a b inc <|> charExpand a b inc)
      where
        sequencePart = many1 (satisfy isAlphaNum)

    charExpand a b inc = do
        x <- fromAlpha a
        y <- fromAlpha b
        return . map (:[]) $ enum x y inc

    numExpand a b inc = do
        x <- fromNumber a
        y <- fromNumber b
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
