{-# LANGUAGE FlexibleContexts #-}
-- | Builder-based parsing. This is useful for parsing Bash\'s complicated
-- words.
module Language.Bash.Parse.Builder
    ( -- * Builders
      Builder
    , fromChar
    , fromString
    , toString
      -- * Monoid parsing
    , (<+>)
    , many
    , many1
      -- * Characters
    , oneOf
    , noneOf
    , char
    , anyChar
    , satisfy
    , string
      -- * Spans
    , span
    , matchedPair
    ) where

import           Prelude                hiding (span)

import           Control.Applicative    ((<|>), liftA2)
import           Data.Monoid            (Endo (..))
import           Text.Parsec            (ParsecT, Stream)
import qualified Text.Parsec.Char       as P
import qualified Text.Parsec.Prim       as P

infixr 4 <+>

-- | An efficient 'String' builder.
type Builder = Endo String

-- | Construct a 'Builder' from a 'Char'.
fromChar :: Char -> Builder
fromChar = Endo . showChar

-- | Construct a 'Builder' from a 'String'.
fromString :: String -> Builder
fromString = Endo . showString

-- | Convert a 'Builder' to a 'String'.
toString :: Builder -> String
toString = flip appEndo ""

-- | Append two monoidal results.
(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 mappend

-- | Concat zero or more monoidal results.
many :: Monoid a => ParsecT s u m a -> ParsecT s u m a
many = fmap mconcat . P.many

-- | Concat one or more monoidal results.
many1 :: Monoid a => ParsecT s u m a -> ParsecT s u m a
many1 = fmap mconcat . P.many1

-- | 'Builder' version of 'P.oneOf'.
oneOf :: Stream s m Char => [Char] -> ParsecT s u m Builder
oneOf cs = fromChar <$> P.oneOf cs

-- | 'Builder' version of 'P.noneOf'.
noneOf :: Stream s m Char => [Char] -> ParsecT s u m Builder
noneOf cs = fromChar <$> P.noneOf cs

-- | 'Builder' version of 'P.char'.
char :: Stream s m Char => Char -> ParsecT s u m Builder
char c = fromChar c <$ P.char c

-- | 'Builder' version of 'P.anyChar'.
anyChar :: Stream s m Char => ParsecT s u m Builder
anyChar = fromChar <$> P.anyChar

-- | 'Builder' version of 'P.satisfy'.
satisfy :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Builder
satisfy p = fromChar <$> P.satisfy p

-- | 'Builder' version of 'P.string'.
string :: Stream s m Char => String -> ParsecT s u m Builder
string s = fromString s <$ P.string s

-- | @span start end escape@ parses a span of text starting with @start@ and
-- ending with @end@, with possible @escape@ sequences inside.
span
    :: Stream s m Char
    => Char
    -> Char
    -> ParsecT s u m Builder
    -> ParsecT s u m Builder
span start end esc = char start *> many inner <* char end
  where
    inner = esc <|> satisfy (/= end)

-- | @matchedPair start end escape@ parses @span start end escape@, including
-- the surrounding @start@ and @end@ characters.
matchedPair
    :: Stream s m Char
    => Char
    -> Char
    -> ParsecT s u m Builder
    -> ParsecT s u m Builder
matchedPair start end esc = char start <+> many inner <+> char end
  where
    inner = esc <|> satisfy (/= end)
