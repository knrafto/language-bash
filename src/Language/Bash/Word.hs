{-# LANGUAGE
    DeriveDataTypeable
  , CPP
  , OverloadedStrings
  , RecordWildCards
  , TypeSynonymInstances
  #-}
-- | Bash words and substitutions.
module Language.Bash.Word
    (
      -- * Words
      Word
    , Span(..)
      -- * Parameters
    , Parameter(..)
    , ParamSubst(..)
    , AltOp(..)
    , LetterCaseOp(..)
    , Direction(..)
      -- * Process
    , ProcessSubstOp(..)
      -- * Manipulation
    , stringToWord
    , wordToString
    , unquote
    ) where

#if __GLASGOW_HASKELL__ >= 710
import Prelude hiding ((<>), Word)
#else
import Data.Traversable (traverse)
#endif

import           Data.Data        (Data)
import           Data.Typeable    (Typeable)
import           Text.PrettyPrint

import           Language.Bash.Operator
import           Language.Bash.Pretty

-- | A Bash word, broken up into logical spans.
type Word = [Span]

-- | An individual unit of a word.
data Span
      -- | A normal character.
    = Char Char
      -- | An escaped character.
    | Escape Char
      -- | A single-quoted string.
    | Single Word
      -- | A double-quoted string.
    | Double Word
      -- | A ANSI C string.
    | ANSIC Word
      -- | A locale-translated string.
    | Locale Word
      -- | A backquote-style command substitution.
      -- To extract the command string, 'unquote' the word inside.
    | Backquote Word
      -- | A parameter substitution.
    | ParamSubst ParamSubst
      -- | An arithmetic substitution.
    | ArithSubst String
      -- | A command substitution.
    | CommandSubst String
      -- | A process substitution.
    | ProcessSubst ProcessSubstOp String
    deriving (Data, Eq, Read, Show, Typeable)

instance Pretty Span where
    pretty (Char c)           = char c
    pretty (Escape c)         = "\\" <> char c
    pretty (Single w)         = "\'" <> pretty w <> "\'"
    pretty (Double w)         = "\"" <> pretty w <> "\""
    pretty (ANSIC w)          = "$\'" <> pretty w <> "\'"
    pretty (Locale w)         = "$\"" <> pretty w <> "\""
    pretty (Backquote w)      = "`" <> pretty w <> "`"
    pretty (ParamSubst s)     = pretty s
    pretty (ArithSubst s)     = "$((" <> text s <> "))"
    pretty (CommandSubst s)   = "$(" <> text s <> ")"
    pretty (ProcessSubst c s) = pretty c <> "(" <> text s <> ")"

    prettyList = hcat . map pretty

-- | A parameter name an optional subscript.
data Parameter = Parameter String (Maybe Word)
    deriving (Data, Eq, Read, Show, Typeable)

instance Pretty Parameter where
    pretty (Parameter s sub) = text s <> subscript sub
      where
        subscript Nothing  = empty
        subscript (Just w) = "[" <> pretty w <> "]"

-- | A parameter substitution.
data ParamSubst
    = Bare
        { -- | The parameter to substitute.
          parameter         :: Parameter
        }
    | Brace
        { -- | Use indirect expansion.
          indirect          :: Bool
        , parameter         :: Parameter
        }
    | Alt
        { indirect          :: Bool
        , parameter         :: Parameter
          -- | Test for both existence and null values.
        , testNull          :: Bool
          -- | The operator.
        , altOp             :: AltOp
          -- | The alternate word.
        , altWord           :: Word
        }
    | Substring
        { indirect          :: Bool
        , parameter         :: Parameter
          -- | The substring offset.
        , subOffset         :: Word
          -- | The substring length, if any.
        , subLength         :: Word
        }
    | Prefix
        { -- | The variable prefix.
          prefix            :: String
          -- | Either @\@@ of @*@.
        , modifier          :: Char
        }
    | Indices
        { parameter         :: Parameter
        }
    | Length
        { parameter         :: Parameter
        }
    | Delete
        { indirect          :: Bool
        , parameter         :: Parameter
          -- | Replace the longest match instead of the shortest match.
        , longest           :: Bool
          -- | Where to delete from.
        , deleteDirection   :: Direction
          -- | The replacement pattern.
        , pattern           :: Word
        }
    | Replace
        { indirect          :: Bool
        , parameter         :: Parameter
          -- | Replace all occurences.
        , replaceAll        :: Bool
          -- | Where to replace.
        , replaceDirection  :: Maybe Direction
        , pattern           :: Word
          -- | The replacement string.
        , replacement       :: Word
        }
    | LetterCase
        { indirect          :: Bool
        , parameter         :: Parameter
          -- | Convert to lowercase, not uppercase.
        , letterCaseOp      :: LetterCaseOp
          -- | Convert all characters, not only the starts of words.
        , convertAll        :: Bool
        , pattern           :: Word
        }
    deriving (Data, Eq, Read, Show, Typeable)

prettyParameter :: Bool -> Parameter -> Doc -> Doc
prettyParameter bang param suffix =
    "${" <> (if bang then "!" else empty) <> pretty param <> suffix <> "}"

twiceWhen :: Bool -> Doc -> Doc
twiceWhen False d = d
twiceWhen True  d = d <> d

instance Pretty ParamSubst where
    pretty Bare{..}       = "$" <> pretty parameter
    pretty Brace{..}      = prettyParameter indirect parameter empty
    pretty Alt{..}        = prettyParameter indirect parameter $
        (if testNull then ":" else empty) <>
        pretty altOp <>
        pretty altWord
    pretty Substring{..}  = prettyParameter indirect parameter $
        ":" <> pretty subOffset <>
        (if null subLength then empty else ":") <> pretty subLength
    pretty Prefix{..}     = "${!" <> text prefix <> char modifier <> "}"
    pretty Indices{..}    = prettyParameter True parameter empty
    pretty Length{..}     = "${#" <> pretty parameter <> "}"
    pretty Delete{..}     = prettyParameter indirect parameter $
        twiceWhen longest (pretty deleteDirection) <>
        pretty pattern
    pretty Replace{..}    = prettyParameter indirect parameter $
        "/" <>
        (if replaceAll then "/" else empty) <>
        pretty replaceDirection <>
        pretty pattern <>
        "/" <>
        pretty replacement
    pretty LetterCase{..} = prettyParameter indirect parameter $
        twiceWhen convertAll (pretty letterCaseOp) <>
        pretty pattern

-- | An alternation operator.
data AltOp
    = AltDefault  -- ^ '-', ':-'
    | AltAssign   -- ^ '=', ':='
    | AltError    -- ^ '?', ':?'
    | AltReplace  -- ^ '+', ':+'
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded)

instance Operator AltOp where
    operatorTable = zip [minBound .. maxBound] ["-", "=", "?", "+"]

instance Pretty AltOp where
    pretty = prettyOperator

-- | A letter case operator.
data LetterCaseOp
    = ToLower
    | ToUpper
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded)

instance Operator LetterCaseOp where
    operatorTable = zip [ToLower, ToUpper] [",", "^"]

instance Pretty LetterCaseOp where
    pretty = prettyOperator

-- | A string direction.
data Direction
    = Front
    | Back
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded)

instance Pretty Direction where
    pretty Front = "#"
    pretty Back  = "%"

-- | A process substitution.
data ProcessSubstOp
    = ProcessIn   -- ^ @\<@
    | ProcessOut  -- ^ @\>@
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded)

instance Operator ProcessSubstOp where
    operatorTable = zip [ProcessIn, ProcessOut] ["<", ">"]

instance Pretty ProcessSubstOp where
    pretty = prettyOperator

-- | Convert a string to an unquoted word.
stringToWord :: String -> Word
stringToWord = map Char

-- | If a word is a plain, unquoted string (e.g. the result of @stringToWord@),
-- returns @Just@ that string; otherwise, returns @Nothing@.
wordToString :: Word -> Maybe String
wordToString = traverse spanToChar
  where
    spanToChar (Char c) = Just c
    spanToChar _        = Nothing

-- | Remove all quoting characters from a word.
unquote :: Word -> String
unquote = render . unquoteWord
  where
    unquoteWord = hcat . map unquoteSpan

    unquoteSpan (Char c)   = char c
    unquoteSpan (Escape c) = char c
    unquoteSpan (Single w) = unquoteWord w
    unquoteSpan (Double w) = unquoteWord w
    unquoteSpan (ANSIC w)  = unquoteWord w
    unquoteSpan (Locale w) = unquoteWord w
    unquoteSpan s          = pretty s
