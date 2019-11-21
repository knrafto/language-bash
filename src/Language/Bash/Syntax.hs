{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards, DeriveGeneric #-}
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
    --
    , BashDoc(..)
    , toBashDoc
    ) where

import Prelude hiding ((<>), Word)
import qualified Prelude

import Data.Data        (Data)
import Data.List (intersperse)
import Data.String (IsString(..))
import Data.Typeable    (Typeable)
import GHC.Generics     (Generic)
import Text.PrettyPrint

import Language.Bash.Cond     (CondExpr)
import Language.Bash.Operator
import Language.Bash.Pretty
import Language.Bash.Word

data BashDoc = BashDoc Doc Doc [Redir]

instance Show BashDoc where
    show (BashDoc line rest hds) = "BashDoc " ++ show (show line) ++ " " ++ show (show rest) ++ " " ++ show hds

instance IsString BashDoc where
    fromString xs = let
        (line:rest) = lines xs
        in BashDoc (text line) (vcat $ map text rest) []

instance Monoid BashDoc where
    mempty = BashDoc empty empty []

instance Semigroup BashDoc where
    BashDoc line1 rest1 hds1 <> BashDoc line2 rest2 hds2 = case (rest1 == empty, rest2 == empty) of
        (False, False) -> BashDoc line1 (rest1 <+> line2 $+$ prettyHeredocs hds1 $+$ rest2) hds2
        (False, True ) -> BashDoc line1 (rest1 <+> line2                                  ) (hds1 ++ hds2)
        (True , False) -> BashDoc (line1 <+> line2) (prettyHeredocs hds1 $+$ rest2) hds2
        (True , True ) -> BashDoc (line1 <+> line2) empty (hds1 ++ hds2)

instance Pretty BashDoc where
    pretty (BashDoc line rest hds) = line $+$ rest $+$ prettyHeredocs hds

-- | A utility class for pretty printing without heredocs
class ToBashDoc a where
    toBashDoc :: a -> BashDoc

prettyHeredocs :: [Redir] -> Doc
prettyHeredocs [] = empty
prettyHeredocs rs = mconcat (map prettyHeredoc rs)
    where
        prettyHeredoc Heredoc{..} = pretty hereDocument <> text heredocDelim
        prettyHeredoc _ = empty

-- | Indent by 4 columns.
indent :: Pretty a => a -> Doc
indent = nest 4 . pretty

-- | Render a @do...done@ block.
doDone :: Pretty a => Doc -> a -> BashDoc
doDone header body = BashDoc header ("do" $+$ indent body $+$ "done") []

-- | A Bash command with redirections.
data Command = Command ShellCommand [Redir]
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty Command where
    pretty = pretty . toBashDoc

instance ToBashDoc Command where
    toBashDoc (Command c rs) = toBashDoc c Prelude.<> BashDoc (pretty rs) empty (filter isHeredoc rs)
        where
            isHeredoc (Heredoc {}) = True
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
    pretty = pretty . toBashDoc

instance ToBashDoc ShellCommand where
    toBashDoc (SimpleCommand as ws)  = BashDoc (pretty as <+> pretty ws) empty []
    toBashDoc (AssignBuiltin w args) = BashDoc (pretty w <+> pretty args) empty []
    toBashDoc (FunctionDef name l)   = BashDoc (text name <+> "()") (pretty (Group l)) []
    toBashDoc (Coproc name c)        = BashDoc ("coproc" <+> text name) empty [] Prelude.<> toBashDoc c
    toBashDoc (Subshell l)           = BashDoc "( " empty [] Prelude.<> toBashDoc l Prelude.<> BashDoc " )" empty []
    toBashDoc (Group l)              = BashDoc "{" (indent l $+$ "}") []
    toBashDoc (Arith s)              = BashDoc ("((" <> text s <> "))") empty []
    toBashDoc (Cond e)               = BashDoc ("[[" <+> pretty e <+> "]]") empty []
    toBashDoc (For w ws l)           = doDone ("for" <+> pretty w <+> pretty ws <> ";") l
    toBashDoc (ArithFor s l)         = doDone ("for" <+> "((" <> text s <> "))") l
    toBashDoc (Select w ws l)        = doDone ("select" <+> pretty w <+> pretty ws <> ";") l
    toBashDoc (Case w cs)            = BashDoc ("case" <+> pretty w <+> "in") ((vcat $ map indent cs) $+$ "esac") []
    toBashDoc (If p t f)             = BashDoc ("if" <+> pretty p <+> "then") (indent t $+$ pretty (fmap (\l -> "else" $+$ indent l) f) $+$ "fi") []
    toBashDoc (Until p l)            = doDone ("until" <+> pretty p) l
    toBashDoc (While p l)            = doDone ("while" <+> pretty p) l

-- | A word list or @\"$\@\"@.
data WordList
    = Args
    | WordList [Word]
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty WordList where
    pretty Args          = empty
    pretty (WordList ws) = "in" <+> pretty ws

-- | A single case clause.
data CaseClause = CaseClause [Word] List CaseTerm
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty CaseClause where
    pretty (CaseClause ps l term) =
        hcat (punctuate " | " (map pretty ps)) <> ")" $+$
        indent l $+$
        (indent $ pretty term)

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
        text (if heredocDelimQuoted
              then "'" ++ heredocDelim ++ "'"
              else heredocDelim)

-- | A redirection file descriptor.
data IODesc
      -- | A file descriptor number.
    = IONumber Int
      -- | A variable @{/varname/}@ to allocate a file descriptor for.
    | IOVar String
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty IODesc where
    pretty (IONumber n) = int n
    pretty (IOVar n)    = "{" <> text n <> "}"

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
    pretty = pretty . toBashDoc

instance ToBashDoc List where
    toBashDoc (List as) = mconcat $ map toBashDoc as

-- | A single statement in a list.
data Statement = Statement AndOr ListTerm
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty Statement where
    pretty = pretty . toBashDoc

instance ToBashDoc Statement where
    toBashDoc (Statement l Sequential)   = toBashDoc l Prelude.<> ";"
    toBashDoc (Statement l Asynchronous) = toBashDoc l Prelude.<> " &"

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
    pretty = pretty . toBashDoc

instance ToBashDoc AndOr where
    toBashDoc (Last p)  = toBashDoc p
    toBashDoc (And p a) = toBashDoc p Prelude.<> " && " Prelude.<> toBashDoc a
    toBashDoc (Or p a)  = toBashDoc p Prelude.<> " || " Prelude.<> toBashDoc a

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
    pretty = pretty . toBashDoc

instance ToBashDoc Pipeline where
    toBashDoc (Pipeline{..}) = let
        timed'      = if timed      then "time" else empty
        timedPosix' = if timedPosix then "-p"   else empty
        inverted'   = if inverted   then "!"    else empty
        prefix = BashDoc (timed' <+> timedPosix' <+> inverted') empty []
        in prefix Prelude.<> mconcat (intersperse "|" $ map toBashDoc commands)

-- | An assignment.
data Assign = Assign Parameter AssignOp RValue
    deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty Assign where
    pretty (Assign lhs op rhs) = pretty lhs <> pretty op <> pretty rhs

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
