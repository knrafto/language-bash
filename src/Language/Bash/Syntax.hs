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
      -- * Syntax elements
    , reservedWords
    , assignBuiltins
    , redirOps
    , heredocOps
    , controlOps
    , normalOps
    ) where

-- | A Bash word.
type Word = String

-- | A variable subscript @[...]@.
type Subscript = Word

-- | A Bash command with redirections.
data Command = Command ShellCommand [Redir]
    deriving (Eq, Read, Show)

-- | A redirection.
data Redir
    -- | A redirection.
    = Redir
        { -- | An optional file descriptor.
          redirDesc   :: Maybe IODesc
          -- | The redirection operator.
        , redirOp     :: String
          -- | The redirection target.
        , redirTarget :: Word
        }
    -- | A here document.
    | Heredoc
        { redirOp            :: String
          -- | The here document delimiter.
        , heredocDelim       :: String
          -- | 'True' if the delimiter was quoted.
        , heredocDelimQuoted :: Bool
          -- | The document itself.
        , document           :: String
        }
    deriving (Eq, Read, Show)

-- | A redirection file descriptor.
data IODesc
    -- | A file descriptor number.
    = IONumber Int
    -- | A variable @{/varname/}@ to allocate a file descriptor for.
    | IOVar String
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

-- | An assignment.
data Assign = Assign LValue AssignOp RValue
    deriving (Eq, Read, Show)

-- | The left side of an assignment.
data LValue = LValue String (Maybe Subscript)
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
    | RArray [(Maybe Subscript, Word)]
    deriving (Eq, Read, Show)

-- | Shell reserved words.
reservedWords :: [Word]
reservedWords =
    [ "!", "[[", "]]", "{", "}"
    , "if", "then", "else", "elif", "fi"
    , "case", "esac", "for", "select", "while", "until"
    , "in", "do", "done", "time", "function"
    ]

-- | Shell assignment builtins. These builtins can take assignments as
-- arguments.
assignBuiltins :: [Word]
assignBuiltins =
    [ "alias", "declare", "export", "eval"
    , "let", "local", "readonly", "typeset"
    ]

-- | Redirection operators, not including here document operators.
redirOps :: [String]
redirOps = [">", "<", ">>", ">|", "<>", "<<<", "<&", ">&", "&>", "&>>"]

-- | Here document operators.
heredocOps :: [String]
heredocOps = ["<<", "<<-"]

-- | Shell control operators.
controlOps :: [String]
controlOps =
    [ "(", ")", ";;", ";&", ";;&"
    , "|", "|&", "||", "&&", ";", "&", "\n"
    ]

-- | All normal operators.
normalOps :: [String]
normalOps = redirOps ++ heredocOps ++ controlOps
