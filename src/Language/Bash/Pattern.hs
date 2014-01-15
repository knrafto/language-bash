-- | Bash patterns, which are used for filename expansion, some parameter
-- expansions,
module Language.Bash.Pattern
    ( -- * Patterns
      Pattern
    , compile
    , isLiteral
      -- * Matching
    , MatchOption(..)
    , (=~)
    , match
    , replace
    , replaceAll
    ) where

import Data.Maybe

import Language.Bash.Syntax

infix 4 =~

-- | A Bash pattern.
data Pattern = Pattern

-- | Compile a pattern from a word.
compile :: Word -> Pattern
compile = undefined

-- | Return 'True' if the pattern can only match one string.
isLiteral :: Pattern -> Bool
isLiteral = undefined

-- | Match options.
data MatchOption
    -- | Match only at the beginning of a string.
    = AnchorLeft
    -- | Match only at the end of a string.
    | AnchorRight
    -- | Return the shortest string instead of the longest string that matches
    -- the pattern.
    | ShortestMatch
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Determine if a string fully matches a pattern.
(=~) :: String -> Pattern -> Bool
s =~ p = isJust (match [AnchorLeft, AnchorRight] p s)

-- | Match a string, returning the text before the match, the match itself,
-- and the text after the match.
match :: [MatchOption] -> Pattern -> String -> Maybe (String, String, String)
match = undefined

-- | Replace an occurence
replace
    :: [MatchOption]       -- ^ Match options
    -> Pattern             -- ^ Pattern
    -> (String -> String)  -- ^ Replacement function
    -> String              -- ^ Target string
    -> String
replace opts p f s = case match opts p s of
    Nothing        -> s
    Just (a, b, c) -> a ++ f b ++ c

replaceAll
    :: [MatchOption]       -- ^ Match options
    -> Pattern             -- ^ Pattern
    -> (String -> String)  -- ^ Replacement function
    -> String              -- ^ Target string
    -> String
replaceAll opts p f
    | any (`elem` opts) [AnchorLeft, AnchorRight] = replace opts p f
    | otherwise                                   = go
  where
    go s = case match opts p s of
        Nothing        -> s
        Just (a, b, c) -> a ++ f b ++ go c
