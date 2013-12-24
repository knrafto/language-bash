-- | Shell script types.
module Bash.Types
    ( -- * Commands
      Command(..)
      -- * Redirections
    , Redir(..)
    , RedirTarget
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

import Bash.Word

-- | A Bash command.
data Command
    = Simple SimpleCommand
    | Shell ShellCommand [Redir]
    | FunctionDef String ShellCommand [Redir]
    | Coproc (Maybe String) Command [Redir]
    deriving (Eq, Read, Show)

-- | A redirection.
data Redir = Redir (Maybe RedirTarget) String Word
    deriving (Eq, Read, Show)

-- | A redirection target.
data RedirTarget
    = IONumber Int
    | IOVar String
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
    = Time [Word] Pipeline
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
    | If List List List
    | Until List List
    | While List List
    deriving (Eq, Read, Show)

-- | A single case clause.
data CaseClause = CaseClause [Word] List CaseTerm
    deriving (Eq, Read, Show)

-- | A case clause terminator. A clause can either 'Break' out of the case
-- statement with @;;@, 'FallThrough' to the next clause with @;&@, or
-- 'Continue' by testing the pattern for the next clause with @;;&@.
data CaseTerm
    = Break
    | FallThrough
    | Continue
    deriving (Eq, Read, Show)
