{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | Shell script types.
module Language.Bash.Syntax
    ( -- * Syntax
      -- ** Words
      Word
    , Subscript
      -- ** Commands
    , Command(..)
    , Redir(..)
    , IODesc(..)
    , RedirOp(..)
    , ShellCommand(..)
    , CaseClause(..)
    , CaseTerm(..)
      -- * Lists
    , List(..)
    , Statement(..)
    , ListTerm(..)
    , AndOr(..)
    , Pipeline(..)
      -- * Assignments
    , Assign(..)
    , LValue(..)
    , AssignOp(..)
    , RValue(..)
    ) where

import Text.PrettyPrint

import Language.Bash.Cond   (CondExpr)
import Language.Bash.Pretty

-- | Indent by 4 columns.
indent :: Pretty a => a -> Doc
indent = nest 4 . pretty

-- | Render a @do...done@ block.
doDone :: Pretty a => a -> Doc
doDone a = "do" $+$ indent a $+$ "done"

-- | A Bash word.
type Word = String

-- | A variable subscript @[...]@.
type Subscript = Word

-- | A Bash command with redirections.
data Command = Command ShellCommand [Redir]
    deriving (Eq, Read, Show)

instance Pretty Command where
    pretty (Command c rs) = pretty c <+> pretty rs

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
        { -- | 'True' if the here document was stripped of leading tabs using
          -- the @\<\<-@ operator.
          heredocStrip       :: Bool
          -- | The here document delimiter.
        , heredocDelim       :: String
          -- | 'True' if the delimiter was quoted.
        , heredocDelimQuoted :: Bool
          -- | The document itself.
        , hereDocument       :: String
        }
    deriving (Eq, Read, Show)

instance Pretty Redir where
    pretty Redir{..} =
        pretty redirDesc <> pretty redirOp <> text redirTarget
    pretty Heredoc{..} =
        text (if heredocStrip then "<<-" else "<<") <>
        text (if heredocDelimQuoted
              then "'" ++ heredocDelim ++ "'"
              else heredocDelim) <> "\n" <>
        text hereDocument <> text heredocDelim <> "\n"

    prettyList = foldr f empty
      where
        f a@Redir{}   b = pretty a <+> b
        f a@Heredoc{} b = pretty a <> b

-- | A redirection file descriptor.
data IODesc
    -- | A file descriptor number.
    = IONumber Int
    -- | A variable @{/varname/}@ to allocate a file descriptor for.
    | IOVar String
    deriving (Eq, Read, Show)

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
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | A redirection operator.
instance Pretty RedirOp where
    pretty In         = "<"
    pretty Out        = ">"
    pretty OutOr      = ">|"
    pretty Append     = ">>"
    pretty AndOut     = "&>"
    pretty AndAppend  = "&>>"
    pretty HereString = "<<<"
    pretty InAnd      = "<&"
    pretty OutAnd     = ">&"
    pretty InOut      = "<>"

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
    -- | A @for /word/ in /words/@ command. If @in /words/@ is absent,
    -- the word list defaults to @\"$\@\"@.
    | For Word [Word] List
    -- | An arithmetic @for ((...))@ command.
    | ArithFor String List
    -- | A @select /word/ in /words/@ command. If @in /words/@ is absent,
    -- the word list defaults to @\"$\@\"@.
    | Select Word [Word] List
    -- | A @case@ command.
    | Case Word [CaseClause]
    -- | An @if@ command, with a predicate, consequent, and alternative.
    -- @elif@ clauses are parsed as nested @if@ statements.
    | If List List (Maybe List)
    -- | An @until@ command.
    | Until List List
    -- | A @while@ command.
    | While List List
    deriving (Eq, Read, Show)

instance Pretty ShellCommand where
    pretty (SimpleCommand as ws)  = pretty as <+> pretty ws
    pretty (AssignBuiltin w args) = text w <+> pretty args
    pretty (FunctionDef name l) =
        text name <+> "()" $+$ pretty (Group l)
    pretty (Coproc name c) =
        "coproc" <+> text name <+> pretty c
    pretty (Subshell l) =
        "(" <+> pretty l <+> ")"
    pretty (Group l) =
        "{" $+$ indent l $+$ "}"
    pretty (Arith s) =
        "((" <> text s <> "))"
    pretty (Cond e) =
        "[[" <+> pretty e <+> "]]"
    pretty (For w ws l) =
        "for" <+> text w <+> "in" <+> pretty ws <> ";" $+$ doDone l
    pretty (ArithFor s l) =
        "for" <+> "((" <> text s <> "))" $+$ doDone l
    pretty (Select w ws l) =
        "select" <+> text w <+> "in" <+> pretty ws <> ";" $+$ doDone l
    pretty (Case w cs) =
        "case" <+> text w <+> "in" $+$ indent cs $+$ "esac"
    pretty (If p t f) =
        "if" <+> pretty p <+> "then" $+$ indent t $+$
        pretty (fmap (\l -> "else" $+$ indent l) f) $+$
        "fi"
    pretty (Until p l) =
        "until" <+> pretty p <+> doDone l
    pretty (While p l) =
        "while" <+> pretty p <+> doDone l

-- | A single case clause.
data CaseClause = CaseClause [Word] List CaseTerm
    deriving (Eq, Read, Show)

instance Pretty CaseClause where
    pretty (CaseClause ps l term) =
        hcat (punctuate " | " (map text ps)) <> ")" $+$
        indent l $+$
        pretty term

-- | A case clause terminator.
data CaseTerm
    = Break        -- ^ @;;@
    | FallThrough  -- ^ @;&@
    | Continue     -- ^ @;;&@
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

instance Pretty CaseTerm where
    pretty Break       = ";;"
    pretty FallThrough = ";&"
    pretty Continue    = ";;&"

-- | A compound list of statements.
newtype List = List [Statement]
    deriving (Eq, Read, Show)

instance Pretty List where
    pretty (List as) = pretty as

-- | A single statement in a list.
data Statement = Statement AndOr ListTerm
    deriving (Eq, Read, Show)

instance Pretty Statement where
    pretty (Statement l Sequential)   = pretty l <> ";"
    pretty (Statement l Asynchronous) = pretty l <+> "&"

    prettyList = foldr f empty
      where
        f a@(Statement _ Sequential)   b = pretty a $+$ b
        f a@(Statement _ Asynchronous) b = pretty a <+> b

-- | A statement terminator.
data ListTerm
    -- | The @;@ operator.
    = Sequential
    -- | The @&@ operator.
    | Asynchronous
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

instance Pretty ListTerm where
    pretty Sequential   = ";"
    pretty Asynchronous = "&"

-- | A right-associative list of pipelines.
data AndOr
    -- | The last pipeline of a list.
    = Last Pipeline
    -- | An @&&@ construct.
    | And Pipeline AndOr
    -- | An @||@ construct.
    | Or Pipeline AndOr
    deriving (Eq, Read, Show)

instance Pretty AndOr where
    pretty (Last p)  = pretty p
    pretty (And p a) = pretty p <+> "&&" <+> pretty a
    pretty (Or p a)  = pretty p <+> "||" <+> pretty a

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
    } deriving (Eq, Read, Show)

instance Pretty Pipeline where
    pretty Pipeline{..} =
        (if timed      then "time" else empty) <+>
        (if timedPosix then "-p"   else empty) <+>
        (if inverted   then "!"    else empty) <+>
        pretty commands

-- | An assignment.
data Assign = Assign LValue AssignOp RValue
    deriving (Eq, Read, Show)

instance Pretty Assign where
    pretty (Assign lhs op rhs) = pretty lhs <> pretty op <> pretty rhs

-- | The left side of an assignment.
data LValue = LValue String (Maybe Subscript)
    deriving (Eq, Read, Show)

instance Pretty LValue where
    pretty (LValue name sub) =
        text name <> pretty (fmap (\s -> "[" ++ s ++ "]") sub)

-- | An assignment operator.
data AssignOp
    = Equals      -- ^ @=@
    | PlusEquals  -- ^ @+=@
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

instance Pretty AssignOp where
    pretty Equals     = "="
    pretty PlusEquals = "+="

-- | The right side of an assignment.
data RValue
    -- | A simple word.
    = RValue Word
    -- | An array assignment.
    | RArray [(Maybe Subscript, Word)]
    deriving (Eq, Read, Show)

instance Pretty RValue where
    pretty (RValue w)  = text w
    pretty (RArray rs) = "(" <> hsep (map f rs) <> ")"
      where
        f (sub, w) = pretty (fmap (\s -> "[" ++ s ++ "]=") sub) <> text w
