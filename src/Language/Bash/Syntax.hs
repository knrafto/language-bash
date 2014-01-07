-- | Shell script types.
module Language.Bash.Syntax
    ( -- * Words
      Word
      -- * Commands
    , Command(..)
      -- * Redirections
    , Redir(..)
      -- * Lists
    , List(..)
    , AndOr(..)
    , Pipeline(..)
      -- * Assignments
    , Assign(..)
    , Subscript
    , LValue(..)
    , AssignOp(..)
    , RValue(..)
      -- * Simple commands
    , SimpleCommand(..)
    , AssignArg(..)
      -- * Shell commands
    , ShellCommand(..)
    , CaseClause(..)
    , CaseTerm(..)
    ) where

-- | A Bash word.
type Word = String

-- | A Bash command.
data Command
    = Simple SimpleCommand
    | Shell ShellCommand [Redir]
    | FunctionDef String ShellCommand [Redir]
    | Coproc (Maybe String) Command [Redir]
    deriving (Eq, Read, Show)

-- | A redirection.
data Redir
    = Redir (Maybe Word) String Word
    | Heredoc Word Word
    deriving (Eq, Read, Show)

-- | A compound list of statements, terminated by @&@ or @;@.
newtype List = List [AndOr]
    deriving (Eq, Read, Show)

-- | A list of pipelines separated by @&&@ and @||@.
data AndOr
    = Last Pipeline
    | And Pipeline AndOr
    | Or Pipeline AndOr
    deriving (Eq, Read, Show)

-- | A (possibly timed or inverted) pipeline, linked with @|@ or @|&@.
data Pipeline
    = Time Bool Pipeline
    | Invert Pipeline
    | Pipeline [Command]
    deriving (Eq, Read, Show)

-- | An assignment.
data Assign = Assign LValue AssignOp RValue
    deriving (Eq, Read, Show)

-- | A subscript.
type Subscript = Maybe Word

-- | The left side of an assignment.
data LValue = LValue String Subscript
    deriving (Eq, Read, Show)

-- | An assignment operator (@=@ or @+=@).
data AssignOp = Equals | PlusEquals
    deriving (Eq, Read, Show)

-- | The right side of an assignment.
data RValue
    = RValue Word
    | RArray [(Subscript, Word)]
    deriving (Eq, Read, Show)

-- | A simple command preceded by assignments. Some builtins (namely @alias@,
-- @declare@, @export@, @eval@, @let@, @local@, @readonly@, and @typeset@),
-- can also accept assignments as arguments.
data SimpleCommand
    = SimpleCommand [Assign] [Word] [Redir]
    | AssignCommand Word [AssignArg] [Redir]
    deriving (Eq, Read, Show)

-- | An assignment or word argument.
data AssignArg
    = AssignArg Assign
    | WordArg Word
    deriving (Eq, Read, Show)

-- | A compound command.
data ShellCommand
    = Subshell List
    | Group List
    | Arith String
    | Cond [Word]
    | For Word [Word] List
    | ArithFor String List
    | Select Word [Word] List
    | Case Word [CaseClause]
    | If [(List, List)] (Maybe List)
    | Until List List
    | While List List
    deriving (Eq, Read, Show)

-- | A single case clause.
data CaseClause = CaseClause [Word] List CaseTerm
    deriving (Eq, Read, Show)

-- | A case clause terminator.
data CaseTerm
    -- | @;;@
    = Break
    -- | @;&@
    | FallThrough
    -- | @;;&@
    | Continue
    deriving (Eq, Read, Show)
