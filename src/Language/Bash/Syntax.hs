{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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

import Text.PrettyPrint

import Language.Bash.Cond     (CondExpr)
import Language.Bash.Operator
import Language.Bash.Pretty
import Language.Bash.Word

-- | Indent by 4 columns.
indent :: Pretty a => a -> Doc
indent = nest 4 . pretty

-- | Render a @do...done@ block.
doDone :: Pretty a => a -> Doc
doDone a = "do" $+$ indent a $+$ "done"

-- | A Bash command with redirections.
data Command = Command ShellCommand [Redir]
    deriving (Eq, Read, Show)

instance Pretty Command where
    pretty (Command c rs) = pretty c <+> pretty rs

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
    deriving (Eq, Read, Show)

instance Pretty ShellCommand where
    pretty (SimpleCommand as ws)  = pretty as <+> pretty ws
    pretty (AssignBuiltin w args) = pretty w <+> pretty args
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
        "for" <+> pretty w <+> pretty ws <> ";" $+$ doDone l
    pretty (ArithFor s l) =
        "for" <+> "((" <> text s <> "))" $+$ doDone l
    pretty (Select w ws l) =
        "select" <+> pretty w <+> pretty ws <> ";" $+$ doDone l
    pretty (Case w cs) =
        "case" <+> pretty w <+> "in" $+$ indent cs $+$ "esac"
    pretty (If p t f) =
        "if" <+> pretty p <+> "then" $+$ indent t $+$
        pretty (fmap (\l -> "else" $+$ indent l) f) $+$
        "fi"
    pretty (Until p l) =
        "until" <+> pretty p <+> doDone l
    pretty (While p l) =
        "while" <+> pretty p <+> doDone l

-- | A word list or @\"$\@\"@.
data WordList
    = Args
    | WordList [Word]
    deriving (Eq, Read, Show)

instance Pretty WordList where
    pretty Args          = empty
    pretty (WordList ws) = "in" <+> pretty ws

-- | A single case clause.
data CaseClause = CaseClause [Word] List CaseTerm
    deriving (Eq, Read, Show)

instance Pretty CaseClause where
    pretty (CaseClause ps l term) =
        hcat (punctuate " | " (map pretty ps)) <> ")" $+$
        indent l $+$
        pretty term

-- | A case clause terminator.
data CaseTerm
    = Break        -- ^ @;;@
    | FallThrough  -- ^ @;&@
    | Continue     -- ^ @;;&@
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

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
    deriving (Eq, Read, Show)

instance Pretty Redir where
    pretty Redir{..} =
        pretty redirDesc <> pretty redirOp <> pretty redirTarget
    pretty Heredoc{..} =
        pretty heredocOp <>
        text (if heredocDelimQuoted
              then "'" ++ heredocDelim ++ "'"
              else heredocDelim) <> "\n" <>
        pretty hereDocument <> text heredocDelim <> "\n"

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

instance Operator RedirOp where
    operatorTable = zip [minBound .. maxBound]
        ["<", ">", ">|", ">>", "&>", "&>>", "<<<", "<&", ">&", "<>"]

instance Pretty RedirOp where
    pretty = prettyOperator

-- | A here document operator.
data HeredocOp
    = Here       -- ^ @\<\<@
    | HereStrip  -- ^ @\<\<-@
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Operator HeredocOp where
    operatorTable = zip [Here, HereStrip] ["<<", "<<-"]

instance Pretty HeredocOp where
    pretty = prettyOperator

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
    = Sequential    -- ^ @;@
    | Asynchronous  -- ^ @&@
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

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
        hcat (punctuate " | " (map pretty commands))

-- | An assignment.
data Assign = Assign Parameter AssignOp RValue
    deriving (Eq, Read, Show)

instance Pretty Assign where
    pretty (Assign lhs op rhs) = pretty lhs <> pretty op <> pretty rhs

-- | An assignment operator.
data AssignOp
    = Equals      -- ^ @=@
    | PlusEquals  -- ^ @+=@
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

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
    deriving (Eq, Read, Show)

instance Pretty RValue where
    pretty (RValue w)  = pretty w
    pretty (RArray rs) = "(" <> hsep (map f rs) <> ")"
      where
        f (Nothing , w) = pretty w
        f (Just sub, w) = "[" <> pretty sub <> "]=" <> pretty w
