{-# LANGUAGE OverloadedStrings #-}
-- | Bash input parsing.
module Bash.Parse
    ( parse
    ) where

import           Control.Applicative    hiding (many)
import           Control.Monad
import           Data.Char
import           Data.Either
import           Data.Functor.Identity
import           Text.Parsec.Char       hiding (newline)
import           Text.Parsec.Combinator hiding (optional)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.Pos
import           Text.Parsec.Prim       hiding (parse)

import qualified Bash.Parse.Internal    as I
import           Bash.Parse.Packrat
import           Bash.Types
import           Bash.Word

data U = U { postHeredoc :: Maybe (State D U) }

type Parser = ParsecT D U Identity

parse :: SourceName -> String -> Either ParseError List
parse source = runParser script (U Nothing) source . pack (initialPos source)

-------------------------------------------------------------------------------
-- Basic parsers
-------------------------------------------------------------------------------

-- | Parse a word satisfying a predicate.
satisfyWord :: (Word -> Bool) -> Parser Word
satisfyWord p = do
    w <- anyWord
    if p w then return w else unexpected (show (toString w))

-- | Parse an operator satisfying a predicate.
satisfyOperator :: (String -> Bool) -> Parser String
satisfyOperator p = do
    op <- anyOperator
    if p op then return op else unexpected (show op)

-- | Parse the given word.
word :: Word -> Parser Word
word w = satisfyWord (== w)

-- | Parse a reversed word.
reservedWord :: Parser Word
reservedWord = satisfyWord (`elem` I.reservedWords)

-- | Parse a word that is not reserved.
unreservedWord :: Parser Word
unreservedWord = satisfyWord (`notElem` I.reservedWords)

-- | Parse an assignment builtin.
assignBuiltin :: Parser Word
assignBuiltin = satisfyWord (`elem` I.assignBuiltins)

-- | Parse a variable name.
name :: Parser String
name = do
    n <- toString <$> unreservedWord
    case n of
        c:cs | isNameStart c && all isNameLetter cs -> return n
        _                                           -> unexpected (show n)
  where
    isNameStart  c = isAlpha c    || c == '_'
    isNameLetter c = isAlphaNum c || c == '_'

-- | Parse a given operator.
operator :: String -> Parser String
operator op = satisfyOperator (== op)

-- | Parse a non-heredoc redirection operator.
redirOp :: Parser String
redirOp = satisfyOperator (`elem` I.redirOps)

-- | Get the next line of input.
line :: Parser String
line = lookAhead anyChar *> many (satisfy (/= '\n')) <* optional (char '\n')

-- | Parse the next here document.
heredoc :: Bool -> String -> Parser String
heredoc strip end = do
    (h, s) <- lookAhead duck
    setState $ U (Just s)
    return h
  where
    process = if strip then dropWhile (== '\t') else id

    duck = do
        u <- getState
        case postHeredoc u of
            Nothing -> () <$ line
            Just s  -> () <$ setParserState s
        h <- unlines <$> heredocLines
        s <- getParserState
        return (h, s)

    heredocLines = do
        l <- process <$> line
        if l == end then return [] else (l :) <$> heredocLines

-- | Parse a newline, skipping any here documents.
newline :: Parser String
newline = do
    _ <- char '\n'
    u <- getState
    case postHeredoc u of
        Nothing -> return ()
        Just s  -> () <$ setParserState s
    setState $ U Nothing
    return "\n"

-- | Skip a list terminator.
listTerm :: Parser ()
listTerm = term *> newlineList
  where
    term = newline
       </> operator ";"
       </> operator "&"

-- | Skip zero or more newlines.
newlineList :: Parser ()
newlineList = skipMany newline

-------------------------------------------------------------------------------
-- Simple commands
-------------------------------------------------------------------------------

-- | Skip a redirection.
redir :: Parser Redir
redir = Redir <$> redirWord <*> redirOp <*> anyWord
    </> Redir <$> redirWord <*> operator "<<"  <*> heredocWord False
    </> Redir <$> redirWord <*> operator "<<-" <*> heredocWord True
  where
    heredocWord strip = do
        w <- anyWord
        h <- heredoc strip (unquote w)
        return (fromString h)  -- TODO

-- | Skip a list of redirections.
redirList :: Parser [Redir]
redirList = many redir

-- | Parse part of a command.
commandParts :: Parser a -> Parser ([a], [Redir])
commandParts p = partitionEithers <$> many part
  where
    part = Left  <$> p
       </> Right <$> redir

-- | Parse a simple command.
simpleCommand :: Parser SimpleCommand
simpleCommand = do
    notFollowedBy (toString <$> reservedWord)
    normalCommand </> assignCommand
  where
    normalCommand = do
        (as, rs1) <- commandParts assign
        (ws, rs2) <- commandParts anyWord
        guard (not $ null as && null ws)
        return $ SimpleCommand as ws (rs1 ++ rs2)

    assignCommand = do
        rs1 <- redirList
        w <- assignBuiltin
        (args, rs2) <- commandParts assignArg
        return $ AssignCommand w args (rs1 ++ rs2)

    assignArg = AssignArg <$> assign
            </> WordArg   <$> anyWord

-------------------------------------------------------------------------------
-- Lists
-------------------------------------------------------------------------------

-- | Parse a pipeline.
pipelineCommand :: Parser Pipeline
pipelineCommand = time
              </> invert
              </> pipeline1
  where
    invert = Invert <$ word "!" <*> pipeline0

    pipeline0 = Pipeline <$> command `sepBy`  pipelineSep
    pipeline1 = Pipeline <$> command `sepBy1` pipelineSep

    pipelineSep = (operator "|" </> operator "|&") <* newlineList

    time = Time <$ word "time" <*> timeFlag <*> (invert </> pipeline0)

    timeFlag = True <$ word "-p"
           </> pure False

-- | Parse a compound list of commands.
compoundList :: Parser List
compoundList = List <$ newlineList <*> list
  where
    list = andOr `sepEndBy1` listTerm

    andOr = do
        p <- pipelineCommand
        let rest = And p <$ operator "&&" <* newlineList <*> andOr
               </> Or  p <$ operator "||" <* newlineList <*> andOr
        rest </> pure (Last p)

-- | Parse a possible empty compound list of commands.
inputList :: Parser List
inputList = newlineList *> option (List []) compoundList

-- | Parse a command group, wrapped either in braces or in a @do...done@ block.
doGroup :: Parser List
doGroup = word "do" *> compoundList <* word "done"
      </> word "{"  *> compoundList <* word "}"

-------------------------------------------------------------------------------
-- Compound commands
-------------------------------------------------------------------------------

-- | Parse a compound command.
shellCommand :: Parser ShellCommand
shellCommand = group
           </> ifCommand
           </> caseCommand
           </> forCommand
           </> whileCommand
           </> untilCommand
           </> selectCommand
           </> condCommand
           </> arithCommand
           </> subshell

-- | Parse a @case@ command.
caseCommand :: Parser ShellCommand
caseCommand = Case <$ word "case"
          <*> anyWord <* newlineList
          <*  word "in" <* newlineList
          <*> clauses
  where
    clauses = [] <$ word "esac"
          </> do p <- pattern
                 c <- inputList
                 nextClause (CaseClause p c)

    nextClause f = (:) <$> (f <$> clauseTerm) <* newlineList <*> clauses
               </> [f Break] <$ newlineList <* word "esac"

    pattern = optional (operator "(")
           *> anyWord `sepBy` operator "|"
           <* operator ")"

    clauseTerm = Break       <$ operator ";;"
             </> FallThrough <$ operator ";&"
             </> Continue    <$ operator ";;&"

-- | Parse a @while@ command.
whileCommand :: Parser ShellCommand
whileCommand = While <$ word "while"
           <*> compoundList
           <*  word "do" <*> compoundList <* word "done"

-- | Parse an @until@ command.
untilCommand :: Parser ShellCommand
untilCommand = Until <$ word "until"
           <*> compoundList
           <*  word "do" <*> compoundList <* word "done"

-- | Parse a list of words for a @for@ or @select@ command.
wordList :: Parser [Word]
wordList = [] <$ operator ";" <* newlineList
       </> newlineList *> inList
  where
    inList = word "in" *> many anyWord <* listTerm
         </> return []

-- | Parse a @for@ command.
forCommand :: Parser ShellCommand
forCommand = word "for" *> (arithFor_ </> for_)
  where
    arithFor_ = ArithFor
            <$  operator "((" <*> I.arith
            <*  optional listTerm
            <*> doGroup

    for_ = For <$> anyWord <*> wordList <*> doGroup

-- | Parse a @select@ command.
selectCommand :: Parser ShellCommand
selectCommand = Select <$ word "select" <*> anyWord <*> wordList <*> doGroup

-- | Parse an @if@ command.
ifCommand :: Parser ShellCommand
ifCommand = If <$> clauses <*> optional elseClause <* word "fi"
  where
    clauses    = (:) <$> ifClause <*> many elifClause
    clause     = (,) <$> compoundList <* word "then" <*> compoundList
    ifClause   = word "if" *> clause
    elifClause = word "elif" *> clause
    elseClause = word "else" *> compoundList

-- | Parse a subshell command.
subshell :: Parser ShellCommand
subshell = Subshell <$ operator "(" <*> compoundList <* operator ")"

-- | Parse a command group.
group :: Parser ShellCommand
group = Group <$ word "{" <*> compoundList <* word "}"

-- | Parse an arithmetic command.
arithCommand :: Parser ShellCommand
arithCommand = Arith <$ string "((" <*> I.arith <* string "))" <* I.skipSpace

-- | Parse a conditional command.
condCommand :: Parser ShellCommand
condCommand = Cond <$ word "[[" <*> many1 condPart <* word "]]"
  where
    condPart = anyWord
           </> fromString <$> anyOperator

-------------------------------------------------------------------------------
-- Coprocesses
-------------------------------------------------------------------------------

-- | Parse a coprocess command.
coproc :: Parser Command
coproc = word "coproc" *> coprocCommand
  where
    coprocCommand = Coproc <$> optional name
                           <*> (Shell <$> shellCommand <*> pure [])
                           <*> redirList
                </> Coproc Nothing <$> (Simple <$> simpleCommand) <*> pure []

-------------------------------------------------------------------------------
-- Function definitions
-------------------------------------------------------------------------------

-- | Parse a function definition.
functionDef :: Parser Command
functionDef = functionDef1
          </> functionDef2
  where
    functionDef1 = FunctionDef <$ word "function"
               <*> (toString <$> anyWord)
               <*  optional functionParens <*> functionBody
               <*> redirList

    functionDef2 = FunctionDef
               <$> (toString <$> unreservedWord)
               <*  functionParens <*> functionBody
               <*> redirList

    functionParens = operator "(" <* operator ")"

    functionBody = newlineList *> shellCommand

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- | Parse a single command.
command :: Parser Command
command = Shell <$> shellCommand <*> redirList
      </> coproc
      </> functionDef
      </> Simple <$> simpleCommand

-- | Parse an entire script (e.g. a file) as a list of commands.
script :: Parser List
script = I.skipSpace *> inputList <* eof
