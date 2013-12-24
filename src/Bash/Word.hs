{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
-- | Bash words, which make up commands.
module Bash.Word
    ( -- * Words
      Word(..)
    , Span(..)
    , split
      -- * Conversion
    , fromString
    , toString
    , unquote
    ) where

import           Data.Monoid
import qualified Data.String

-- | A Bash word.
newtype Word = Word [Span]
    deriving (Eq, Ord, Read, Show, Monoid)

instance Data.String.IsString Word where
    fromString = fromString

-- | One element of a Bash word.
data Span
    -- | An unquoted character.
    = Char Char
    -- | A backslash-escaped character.
    | Escape Char
    -- | A single-quoted string.
    | Single Word
    -- | A double-quoted string.
    | Double Word
    -- | A backquoted command substitution.
    | Backquote String
    -- | A bare @$...@ parameter substitution.
    | Parameter String
    -- | A brace style @${...}@ parameter expansion.
    | BraceParameter Word
    -- | A @$((...))@ arithmetic expansion.
    | ArithSubst String
    -- | A @$(...)@ command substitution.
    | CommandSubst String
    -- | A @\<(...)@ or @\>(...)@ process substitution.
    | ProcessSubst Char String
    deriving (Eq, Ord, Read, Show)

-- | Split a word into individual spans.
split :: Word -> [Span]
split (Word ss) = ss

-- | Convert a string to an unquoted word. No characters are interpreted
-- specially.
fromString :: String -> Word
fromString = Word . map Char

-- | Show a list of values with a function.
showMany :: (a -> ShowS) -> [a] -> ShowS
showMany f = foldr (.) id . map f

-- | Convert a word into a rendered string.
toString :: Word -> String
toString = ($ "") . go
  where
    go = showMany showSpan . split

    showSpan = \case
        Char c           -> showChar c
        Escape c         -> showChar '\\' . showChar c
        Single w         -> wrap "\'"  "\'" (go w)
        Double w         -> wrap "\""  "\"" (go w)
        Backquote s      -> wrap "`"   "`"  (showString s)
        Parameter s      -> showChar '$' . showString s
        BraceParameter w -> wrap "${"  "}"  (go w)
        ArithSubst s     -> wrap "$((" "))" (showString s)
        CommandSubst s   -> wrap "$("  ")"  (showString s)
        ProcessSubst c s -> showChar c . wrap "(" ")" (showString s)

    wrap start end f = showString start . f . showString end

-- | Remove all quoting from a word. This unquotes quoted or escaped
-- characters, and removes all expansions and substitutions.
unquote :: Word -> String
unquote = ($ "") . go
  where
    go = showMany showSpan . split

    showSpan = \case
        Char c   -> showChar c
        Escape c -> showChar c
        Single w -> go w
        Double w -> go w
        _        -> id
