{-# LANGUAGE
    FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , RecordWildCards
  #-}
-- | Memoized packrat parsing, inspired by Edward Kmett\'s
-- \"A Parsec Full of Rats\".
module Language.Bash.Parse.Packrat
    ( -- * Packrat parsing
      (</>)
    , D
    , pack
      -- * Whitespace
    , I.skipSpace
      -- * Words
    , anyWord
    , word
    , redirWord
    , reservedWord
    , unreservedWord
    , assignBuiltin
    , name
    , I.arith
      -- * Operators
    , anyOperator
    , operator
    , redirOp
      -- * Assignments
    , assign
    ) where

import           Control.Applicative
import           Control.Monad.Fix
import           Data.Char
import           Data.Functor.Identity
import           Text.Parsec.Char
import           Text.Parsec.Prim             hiding ((<|>), token)
import           Text.Parsec.Pos

import qualified Language.Bash.Parse.Internal as I
import           Language.Bash.Syntax

infixl 3 </>

-- | Backtracking choice.
(</>) :: Monad m => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
p </> q = try p <|> q

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

-- | Redirection operators, not including heredoc operators.
redirOps :: [String]
redirOps = [">", "<", ">>", ">|", "<>", "<<<", "<&", ">&", "&>", "&>>"]

-- | Heredoc operators.
heredocOps :: [String]
heredocOps = ["<<", "<<-"]

-- | Shell control operators.
controlOps :: [String]
controlOps =
    [ "(", ")", ";;", ";&", ";;&"
    , "|", "|&", "||", "&&", ";", "&", "\n"
    ]

-- | All normal mode operators.
normalOps :: [String]
normalOps = redirOps ++ heredocOps ++ controlOps

-- | A memoized result.
type Result d a = Consumed (Reply d () a)

-- | Build a parser from a field accessor.
rat :: Monad m => (d -> Result d a) -> ParsecT d u m a
rat f = mkPT $ \s0 -> return $
    return . patch s0 <$> f (stateInput s0)
  where
    patch (State _ _ u) (Ok a (State s p _) err) = Ok a (State s p u) err
    patch _             (Error e)                = Error e

-- | Obtain a result from a stateless parser.
womp :: d -> SourcePos -> ParsecT d () Identity a -> Result d a
womp d pos p = fmap runIdentity . runIdentity $
    runParsecT p (State d pos ())

-- | A token.
data Token
    = TWord Word
    | TRedirWord Word

-- | A stream with memoized results.
data D = D
    { _token       :: Result D Token
    , _anyWord     :: Result D Word
    , _redirWord   :: Result D (Maybe Word)
    , _anyOperator :: Result D String
    , _assign      :: Result D Assign
    , _uncons      :: Maybe (Char, D)
    }

instance Monad m => Stream D m Char where
    uncons = return . _uncons

-- | Create a source from a string.
pack :: SourcePos -> String -> D
pack p s = fix $ \d ->
    let result       = womp d p
        _token       = result $ do
            t <- I.word1
            next <- optional (lookAhead anyChar)
            I.skipSpace
            return $ case next of
                Just c | c == '<' || c == '>' -> TRedirWord t
                _                             -> TWord t
        _anyWord     = result $ token >>= \case
            TWord w -> return w
            _       -> empty
        _redirWord   = result $ optional $ token >>= \case
            TRedirWord w -> return w
            _            -> empty
        _anyOperator = result $ I.operator normalOps <* I.skipSpace
        _assign      = result $ I.assign <* I.skipSpace
        _uncons      = case s of
            []     -> Nothing
            (x:xs) -> Just (x, pack (updatePosChar p x) xs)
    in  D {..}

-- | Parse a value satisfying the predicate.
satisfying
    :: (Stream s m t, Show a)
    => ParsecT s u m a
    -> (a -> Bool)
    -> ParsecT s u m a
satisfying a p = do
    t <- a
    if p t then return t else unexpected (show t)

-- | Parse a single token.
token :: Monad m => ParsecT D u m Token
token = rat _token

-- | Parse any word.
anyWord :: Monad m => ParsecT D u m Word
anyWord = rat _anyWord

-- | Parse the given word.
word :: Monad m => Word -> ParsecT D u m Word
word w = anyWord `satisfying` (== w)

-- | Parse a redirection word or number.
redirWord :: Monad m => ParsecT D u m (Maybe Word)
redirWord = rat _redirWord

-- | Parse a reversed word.
reservedWord :: Monad m => ParsecT D u m Word
reservedWord = anyWord `satisfying` (`elem` reservedWords)

-- | Parse a word that is not reserved.
unreservedWord :: Monad m => ParsecT D u m Word
unreservedWord = anyWord `satisfying` (`notElem` reservedWords)

-- | Parse an assignment builtin.
assignBuiltin :: Monad m => ParsecT D u m Word
assignBuiltin = anyWord `satisfying` (`elem` assignBuiltins)

-- | Parse a variable name.
name :: Monad m => ParsecT D u m String
name = do
    n <- unreservedWord
    case n of
        c:cs | isNameStart c && all isNameLetter cs -> return n
        _                                           -> unexpected (show n)
  where
    isNameStart  c = isAlpha c    || c == '_'
    isNameLetter c = isAlphaNum c || c == '_'

-- | Parse any operator.
anyOperator :: Monad m => ParsecT D u m String
anyOperator = rat _anyOperator

-- | Parse a given operator.
operator :: Monad m => String -> ParsecT D u m String
operator op = anyOperator `satisfying` (== op)

-- | Parse a non-heredoc redirection operator.
redirOp :: Monad m => ParsecT D u m String
redirOp = anyOperator `satisfying` (`elem` redirOps)

-- | Parse an assignment.
assign :: Monad m => ParsecT D u m Assign
assign = rat _assign
