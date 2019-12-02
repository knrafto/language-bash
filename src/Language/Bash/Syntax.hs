{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, RecordWildCards, DeriveGeneric #-}
-- | Shell script types.
module Language.Bash.Syntax
    (
      -- * Commands
      Command(..)
    , ShellCommand(..)
    , WordList(..)
    , CaseClause(..)
    , CaseTerm(..)
      -- * Redirections
    , Redir(..)
    , IODesc(..)
    , RedirOp(..)
    , HeredocOp(..)
      -- * Lists
    , List(..)
    , Statement(..)
    , ListTerm(..)
    , AndOr(..)
    , Pipeline(..)
      -- * Assignments
    , Assign(..)
    , AssignOp(..)
    , RValue(..)
    ) where

import Prelude hiding (Word)

import Data.Data        (Data)
import Data.List        (intersperse)
import Data.Semigroup   (Semigroup(..))
import Data.Typeable    (Typeable)
import GHC.Generics     (Generic)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), (<+>), hardline, hcat, hsep, indent, nest, nesting, punctuate, vcat)
import Data.Text.Prettyprint.Doc.Internal (Doc(Empty))

import Language.Bash.Cond     (CondExpr)
import Language.Bash.Operator
import Language.Bash.Pretty
import Language.Bash.Word

-- | The BashDoc monoid is used for building Statements, AndOr or Pipelines.
-- Consider the following situation: We have the following command
--
-- > cat <<EOF
-- > some here doc
-- > EOF
--
-- and we want to pipe its output to another arbitrary command @cmd@.
-- We want this pipeline to look like this:
--
-- > cat <<EOF |
-- > some here doc
-- > EOF
-- > cmd
--
-- Note the @|@ at the end of the first line: If we were simply pretty printing the @cat@ command we had no idea where to insert the pipe symbol.
-- And that's the purpose of BashDoc: We store possible suffixes to such lines, commands and the here documents attached to them separately and do the concatenation in the Semigroup instance of BashDoc.
data BashDoc ann = BashDoc
    (Doc ann) -- ^ The head: This is stuff we want to put before the line break and here documents
    (Doc ann) -- ^ The tail: Everthing which follows the here documents
    (Doc ann) -- ^ Collected here documents

instance Semigroup (BashDoc ann) where
    BashDoc Empty Empty Empty <> y = y
    x <> BashDoc Empty Empty Empty = x
    BashDoc h1 t1 Empty <> BashDoc h2 t2 hds2 = BashDoc h1 (t1 <> h2 <++> t2) hds2
    BashDoc h1 t1 hds1  <> BashDoc h2 t2 hds2 = BashDoc h1 (t1 <> noIndent (h2 $++$ hds1) $++$ t2) hds2
        where
            noIndent doc = nesting $ \i -> nest (- i) doc

instance Monoid (BashDoc ann) where
    mempty = BashDoc mempty mempty mempty
    mappend = (<>)

docOp :: Doc ann -> BashDoc ann
docOp xs = BashDoc xs mempty mempty

prettyBashDoc :: BashDoc ann -> Doc ann
prettyBashDoc (BashDoc h t hds) = h <++> t $++$ hds

-- | A utility class for pretty printing without heredocs
class ToBashDoc a where
    toBashDoc :: a -> BashDoc ann

prettyHeredocs :: [Redir] -> Doc ann
prettyHeredocs [] = mempty
prettyHeredocs rs = mconcat $ intersperse hardline $ map prettyHeredoc rs
    where
        prettyHeredoc Heredoc{..} = pretty hereDocument <> pretty heredocDelim
        prettyHeredoc _ = mempty

-- | Indent by 4 columns.
indent' :: Doc ann -> Doc ann
indent' = indent 4

-- | Render a @do...done@ block.
doDone :: Doc ann -> Doc ann -> Doc ann
doDone header body = header <+> "do" $+$ indent' body $+$ "done"

-- | A Bash command with redirections.
data Command = Command ShellCommand [Redir]
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty Command where
    pretty = prettyBashDoc . toBashDoc

instance ToBashDoc Command where
    toBashDoc (Command c rs) = BashDoc mempty (pretty c <++> pretty rs) (prettyHeredocs $ filter isHeredoc rs)
        where
            isHeredoc Heredoc{..} = True
            isHeredoc _ = False

-- | A Bash command.
data ShellCommand
      -- | A simple command consisting of assignments followed by words.
    = SimpleCommand [Assign] [Word]
      -- | The shell builtins @declare@, @eval@, @export@, @local@, @readonly@,
      -- and @typeset@ can accept both assignments and words as arguments.
    | AssignBuiltin Word [Either Assign Word]
      -- | A function name and definition.
    | FunctionDef String List
      -- | A named coprocess.
    | Coproc String Command
      -- | A @(...)@ list, denoting a subshell.
    | Subshell List
      -- | A @{...}@ list.
    | Group List
      -- | An arithmetic expression.
    | Arith String
      -- | A Bash @[[...]]@ conditional expression.
    | Cond (CondExpr Word)
      -- | A @for /name/ in /words/@ command. If @in /words/@ is absent,
      -- the word list defaults to @\"$\@\"@.
    | For String WordList List
      -- | An arithmetic @for ((...))@ command.
    | ArithFor String List
      -- | A @select /name/ in /words/@ command. If @in /words/@ is absent,
      -- the word list defaults to @\"$\@\"@.
    | Select String WordList List
      -- | A @case@ command.
    | Case Word [CaseClause]
      -- | An @if@ command, with a predicate, consequent, and alternative.
      -- @elif@ clauses are parsed as nested @if@ statements.
    | If List List (Maybe List)
      -- | An @until@ command.
    | Until List List
      -- | A @while@ command.
    | While List List
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty ShellCommand where
    pretty (SimpleCommand as ws)  = pretty as <++> pretty ws
    pretty (AssignBuiltin w args) = pretty w <++> hsep (map (either pretty pretty) args)
    pretty (FunctionDef name l) =
        pretty name <+> "()" $+$ pretty (Group l)
    pretty (Coproc name c) =
        "coproc" <+> pretty name <+> pretty c
    pretty (Subshell l) =
        "(" <+> pretty l <+> ")"
    pretty (Group l) =
        "{" $+$ indent' (pretty l) $+$ "}"
    pretty (Arith s) =
        "((" <> pretty s <> "))"
    pretty (Cond e) =
        "[[" <+> pretty e <+> "]]"
    pretty (For w ws l) =
        doDone ("for" <+> pretty w <+> pretty ws <> ";") (pretty l)
    pretty (ArithFor s l) =
        doDone ("for" <+> "((" <> pretty s <> "))") (pretty l)
    pretty (Select w ws l) =
        doDone ("select" <+> pretty w <+> pretty ws <> ";") (pretty l)
    pretty (Case w cs) =
        "case" <+> pretty w <+> "in" $+$ (vcat $ map (indent' . pretty) cs) $+$ "esac"
    pretty (If p t f) =
        "if" <+> pretty p <+> "then" $+$ indent' (pretty t) $+$
        (maybe mempty (\l -> "else" $+$ indent' (pretty l)) f) $+$
        "fi"
    pretty (Until p l) =
        doDone ("until" <+> pretty p) (pretty l)
    pretty (While p l) =
        doDone ("while" <+> pretty p) (pretty l)

-- | A word list or @\"$\@\"@.
data WordList
    = Args
    | WordList [Word]
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty WordList where
    pretty Args          = mempty
    pretty (WordList ws) = "in" <+> pretty ws

-- | A single case clause.
data CaseClause = CaseClause [Word] List CaseTerm
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty CaseClause where
    pretty (CaseClause ps l term) =
        hcat (punctuate " | " (map pretty ps)) <> ")" $+$
        indent' (pretty l) $+$
        (indent' $ pretty term)

-- | A case clause terminator.
data CaseTerm
    = Break        -- ^ @;;@
    | FallThrough  -- ^ @;&@
    | Continue     -- ^ @;;&@
    deriving (Data, Eq, Ord, Read, Show, Typeable, Bounded, Enum, Generic)

instance Operator CaseTerm where
    operatorTable = zip [minBound .. maxBound] [";;", ";&", ";;&"]

instance Pretty CaseTerm where
    pretty = prettyOperator

-- | A redirection.
data Redir
      -- | A redirection.
    = Redir
        { -- | An optional file descriptor.
          redirDesc   :: Maybe IODesc
          -- | The redirection operator.
        , redirOp     :: RedirOp
          -- | The redirection target.
        , redirTarget :: Word
        }
      -- | A here document.
    | Heredoc
        { -- | The here document operator.
          heredocOp          :: HeredocOp
          -- | The here document delimiter.
        , heredocDelim       :: String
          -- | 'True' if the delimiter was quoted.
        , heredocDelimQuoted :: Bool
          -- | The document itself, if the delimiter was quoted, no expansions
          -- are parsed. If the delimiter was not quoted, parameter, arithmetic
          -- and command substitutions take place.
        , hereDocument       :: Word
        }
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty Redir where
    pretty Redir{..} =
        pretty redirDesc <> pretty redirOp <> pretty redirTarget
    pretty Heredoc{..} =
        pretty heredocOp <>
        pretty (if heredocDelimQuoted
              then "'" ++ heredocDelim ++ "'"
              else heredocDelim)

    prettyList = hsep . map pretty

-- | A redirection file descriptor.
data IODesc
      -- | A file descriptor number.
    = IONumber Int
      -- | A variable @{/varname/}@ to allocate a file descriptor for.
    | IOVar String
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty IODesc where
    pretty (IONumber n) = pretty n
    pretty (IOVar n)    = "{" <> pretty n <> "}"

-- | A redirection operator.
data RedirOp
    = In          -- ^ @\<@
    | Out         -- ^ @\>@
    | OutOr       -- ^ @\>|@
    | Append      -- ^ @\>\>@
    | AndOut      -- ^ @&\>@
    | AndAppend   -- ^ @&\>\>@
    | HereString  -- ^ @\<\<\<@
    | InAnd       -- ^ @\<&@
    | OutAnd      -- ^ @\>&@
    | InOut       -- ^ @\<\>@
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded, Generic)

instance Operator RedirOp where
    operatorTable = zip [minBound .. maxBound]
        ["<", ">", ">|", ">>", "&>", "&>>", "<<<", "<&", ">&", "<>"]

instance Pretty RedirOp where
    pretty = prettyOperator

-- | A here document operator.
data HeredocOp
    = Here       -- ^ @\<\<@
    | HereStrip  -- ^ @\<\<-@
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded, Generic)

instance Operator HeredocOp where
    operatorTable = zip [Here, HereStrip] ["<<", "<<-"]

instance Pretty HeredocOp where
    pretty = prettyOperator

-- | A compound list of statements.
newtype List = List [Statement]
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty List where
    pretty (List as) = pretty as

-- | A single statement in a list.
data Statement = Statement AndOr ListTerm
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty Statement where
    pretty = prettyBashDoc . toBashDoc

    prettyList = foldr f mempty
      where
        f a@(Statement _ Sequential)   b = pretty a $++$ b
        f a@(Statement _ Asynchronous) b = pretty a <++> b

instance ToBashDoc Statement where
    toBashDoc (Statement l Sequential)   = toBashDoc l <> docOp ";"
    toBashDoc (Statement l Asynchronous) = toBashDoc l <> docOp "&"

-- | A statement terminator.
data ListTerm
    = Sequential    -- ^ @;@
    | Asynchronous  -- ^ @&@
    deriving (Data, Eq, Ord, Read, Show, Typeable, Bounded, Enum, Generic)

instance Operator ListTerm where
    operatorTable =
        [ (Sequential  , ";" )
        , (Sequential  , "\n")
        , (Asynchronous, "&" )
        ]

instance Pretty ListTerm where
    pretty = prettyOperator

-- | A right-associative list of pipelines.
data AndOr
      -- | The last pipeline of a list.
    = Last Pipeline
      -- | A @&&@ construct.
    | And Pipeline AndOr
      -- | A @||@ construct.
    | Or Pipeline AndOr
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty AndOr where
    pretty = prettyBashDoc . toBashDoc

instance ToBashDoc AndOr where
    toBashDoc (Last p)  = toBashDoc p
    toBashDoc (And p a) = toBashDoc p <> docOp " &&" <> toBashDoc a
    toBashDoc (Or p a)  = toBashDoc p <> docOp " ||" <> toBashDoc a

-- | A (possibly timed or inverted) pipeline, linked with @|@ or @|&@.
data Pipeline = Pipeline
    { -- | 'True' if the pipeline is timed with @time@.
      timed      :: Bool
      -- | 'True' if the pipeline is timed with the @-p@ flag.
    , timedPosix :: Bool
      -- | 'True' if the pipeline is inverted with @!@.
    , inverted   :: Bool
      -- | A list of commands, separated by @|@, or @|&@.
      -- @command1 |& command2@ is treated as a shorthand for
      -- @command1 2>&1 | command2@.
    , commands   :: [Command]
    } deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty Pipeline where
    pretty = prettyBashDoc . toBashDoc

instance ToBashDoc Pipeline where
    toBashDoc Pipeline{..} = let
        timed'      = if timed      then "time" else mempty
        timedPosix' = if timedPosix then "-p"   else mempty
        inverted'   = if inverted   then "!"    else mempty
        space       = if timed || timedPosix || inverted then " " else mempty
        prefix = BashDoc mempty (timed' <++> timedPosix' <++> inverted' <> space) mempty
        in prefix <> mconcat (intersperse (docOp " |") (map toBashDoc commands))

-- | An assignment.
data Assign = Assign Parameter AssignOp RValue
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty Assign where
    pretty (Assign lhs op rhs) = pretty lhs <> pretty op <> pretty rhs

    prettyList = hsep . map pretty

-- | An assignment operator.
data AssignOp
    = Equals      -- ^ @=@
    | PlusEquals  -- ^ @+=@
    deriving (Data, Eq, Ord, Read, Show, Typeable, Bounded, Enum, Generic)

instance Operator AssignOp where
    operatorTable = zip [Equals, PlusEquals] ["=", "+="]

instance Pretty AssignOp where
    pretty = prettyOperator

-- | The right side of an assignment.
data RValue
      -- | A simple word.
    = RValue Word
      -- | An array assignment, as @(subscript, word)@ pairs.
    | RArray [(Maybe Word, Word)]
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty RValue where
    pretty (RValue w)  = pretty w
    pretty (RArray rs) = "(" <> hsep (map f rs) <> ")"
      where
        f (Nothing , w) = pretty w
        f (Just sub, w) = "[" <> pretty sub <> "]=" <> pretty w
