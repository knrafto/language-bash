-- | Shell script types.
module Language.Bash.Syntax
    ( -- * Words
      Word
      -- * Commands
    , Command(..)
    , ShellCommand(..)
    , CaseClause(..)
    , CaseTerm(..)
      -- * Redirections
    , Redir(..)
      -- * Lists
    , List(..)
    , Statement(..)
    , ListTerm(..)
    , AndOr(..)
    , Pipeline(..)
      -- * Assignments
    , Assign(..)
    , Subscript(..)
    , LValue(..)
    , AssignOp(..)
    , RValue(..)
    ) where

-- | A Bash word.
type Word = String

-- | A Bash command with redirections.
data Command = Command ShellCommand [Redir]
    deriving (Eq, Read, Show)

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
    | Cond [Word]
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

-- | A single case clause.
data CaseClause = CaseClause [Word] List CaseTerm
    deriving (Eq, Read, Show)

-- | A case clause terminator.
data CaseTerm
    -- | The @;;@ operator.
    = Break
    -- | The @;&@ operator.
    | FallThrough
    -- | The @;;&@ operator.
    | Continue
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

-- | A redirection.
data Redir
    -- | A redirection, consisting of an optional number or @{varname}@,
    -- a redirection operator, and a target.
    = Redir (Maybe Word) String Word
    -- | A heredoc, consisting of an operator, a delimiter, whether or not the
    -- delimiter was quoted, and the document itself.
    | Heredoc String String Bool String
    deriving (Eq, Read, Show)

-- | A compound list of statements.
newtype List = List [Statement]
    deriving (Eq, Read, Show)

-- | A single statement in a list.
data Statement = Statement AndOr ListTerm
    deriving (Eq, Read, Show)

-- | A statement terminator.
data ListTerm
    -- | The @;@ operator.
    = Sequential
    -- | The @&@ operator.
    | Asynchronous
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

-- | A right-associative list of pipelines.
data AndOr
    -- | The last pipeline of a list.
    = Last Pipeline
    -- | An @&&@ construct.
    | And Pipeline AndOr
    -- | An @||@ construct.
    | Or Pipeline AndOr
    deriving (Eq, Read, Show)

-- | A (possibly timed or inverted) pipeline, linked with @|@ or @|&@.
data Pipeline
    -- | A timed pipeline, optionally witha @-p@ flag.
    = Time Bool Pipeline
    -- | A pipeline inverted with @!@.
    | Invert Pipeline
    -- | A list of commands, separated by @|@, or @|&@.
    -- @command1 |& command2@ is treated as a shorthand for
    -- @command1 2>&1 | command2@.
    | Pipeline [Command]
    deriving (Eq, Read, Show)

-- | An assignment.
data Assign = Assign LValue AssignOp RValue
    deriving (Eq, Read, Show)

-- | An optional subscript.
newtype Subscript = Subscript (Maybe Word)
    deriving (Eq, Read, Show)

-- | The left side of an assignment.
data LValue = LValue String Subscript
    deriving (Eq, Read, Show)

-- | An assignment operator.
data AssignOp
    -- | The @=@ operator.
    = Equals
    -- | The @+=@ operator.
    | PlusEquals
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

-- | The right side of an assignment.
data RValue
    -- | A simple word.
    = RValue Word
    -- | An array assignment.
    | RArray [(Subscript, Word)]
    deriving (Eq, Read, Show)
