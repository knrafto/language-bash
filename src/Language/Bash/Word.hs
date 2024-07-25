{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , FlexibleInstances
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

import Prelude hiding (Word)

import           Data.Data        (Data)
import           Data.Typeable    (Typeable)
import           GHC.Generics     (Generic)
import           Prettyprinter    (Doc, Pretty(..), hcat, hsep, layoutCompact)
import           Prettyprinter.Render.String (renderString)

import           Language.Bash.Operator

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
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty Span where
    pretty (Char c)           = pretty c
    pretty (Escape c)         = "\\" <> pretty c
    pretty (Single w)         = "\'" <> pretty w <> "\'"
    pretty (Double w)         = "\"" <> pretty w <> "\""
    pretty (ANSIC w)          = "$\'" <> pretty w <> "\'"
    pretty (Locale w)         = "$\"" <> pretty w <> "\""
    pretty (Backquote w)      = "`" <> pretty w <> "`"
    pretty (ParamSubst s)     = pretty s
    pretty (ArithSubst s)     = "$((" <> pretty s <> "))"
    pretty (CommandSubst s)   = "$(" <> pretty s <> ")"
    pretty (ProcessSubst c s) = pretty c <> "(" <> pretty s <> ")"

    prettyList = hcat . map pretty

instance {-# OVERLAPS #-} Pretty [Word] where
    pretty = hsep . map pretty

-- | A parameter name an optional subscript.
data Parameter = Parameter String (Maybe Word)
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty Parameter where
    pretty (Parameter s sub) = pretty s <> subscript sub
      where
        subscript Nothing  = mempty
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
    deriving (Data, Eq, Read, Show, Typeable, Generic)

prettyParameter :: Bool -> Parameter -> Doc ann -> Doc ann
prettyParameter bang param suffix =
    "${" <> (if bang then "!" else mempty) <> pretty param <> suffix <> "}"

twiceWhen :: Bool -> Doc ann -> Doc ann
twiceWhen False d = d
twiceWhen True  d = d <> d

instance Pretty ParamSubst where
    pretty Bare{..}       = "$" <> pretty parameter
    pretty Brace{..}      = prettyParameter indirect parameter mempty
    pretty Alt{..}        = prettyParameter indirect parameter $
        (if testNull then ":" else mempty) <>
        pretty altOp <>
        pretty altWord
    pretty Substring{..}  = prettyParameter indirect parameter $
        ":" <> pretty subOffset <>
        (if null subLength then mempty else ":") <> pretty subLength
    pretty Prefix{..}     = "${!" <> pretty prefix <> pretty modifier <> "}"
    pretty Indices{..}    = prettyParameter True parameter mempty
    pretty Length{..}     = "${#" <> pretty parameter <> "}"
    pretty Delete{..}     = prettyParameter indirect parameter $
        twiceWhen longest (pretty deleteDirection) <>
        pretty pattern
    pretty Replace{..}    = prettyParameter indirect parameter $
        "/" <>
        (if replaceAll then "/" else mempty) <>
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
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded, Generic)

instance Operator AltOp where
    operatorTable = zip [minBound .. maxBound] ["-", "=", "?", "+"]

instance Pretty AltOp where
    pretty = prettyOperator

-- | A letter case operator.
data LetterCaseOp
    = ToLower
    | ToUpper
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded, Generic)

instance Operator LetterCaseOp where
    operatorTable = zip [ToLower, ToUpper] [",", "^"]

instance Pretty LetterCaseOp where
    pretty = prettyOperator

-- | A string direction.
data Direction
    = Front
    | Back
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded, Generic)

instance Pretty Direction where
    pretty Front = "#"
    pretty Back  = "%"

-- | A process substitution.
data ProcessSubstOp
    = ProcessIn   -- ^ @\<@
    | ProcessOut  -- ^ @\>@
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded, Generic)

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
unquote = renderString . layoutCompact . unquoteWord
  where
    unquoteWord = hcat . map unquoteSpan

    unquoteSpan (Char c)   = pretty c
    unquoteSpan (Escape c) = pretty c
    unquoteSpan (Single w) = unquoteWord w
    unquoteSpan (Double w) = unquoteWord w
    unquoteSpan (ANSIC w)  = unquoteWord w
    unquoteSpan (Locale w) = unquoteWord w
    unquoteSpan s          = pretty s
