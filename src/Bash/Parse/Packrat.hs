{-# LANGUAGE
    FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , RecordWildCards
  #-}
-- | Memoized packrat parsing, inspired by Edward Kmett\'s
-- \"A Parsec Full of Rats\".
module Bash.Parse.Packrat
    ( (</>)
    , D
    , pack
    , anyWord
    , redirWord
    , anyOperator
    , assign
    ) where

import           Control.Applicative
import           Control.Monad.Fix
import           Data.Functor.Identity
import           Text.Parsec.Char
import           Text.Parsec.Prim      hiding ((<|>), token)
import           Text.Parsec.Pos

import qualified Bash.Parse.Internal   as I
import           Bash.Word
import           Bash.Types

infixl 3 </>

-- | Backtracking choice.
(</>) :: Monad m => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
p </> q = try p <|> q

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
        _anyOperator = result $ I.operator I.normalOps <* I.skipSpace
        _assign      = result $ I.assign <* I.skipSpace
        _uncons      = case s of
            []     -> Nothing
            (x:xs) -> Just (x, pack (updatePosChar p x) xs)
    in  D {..}

-- | Parse a single token.
token :: Monad m => ParsecT D u m Token
token = rat _token

-- | Parse any word.
anyWord :: Monad m => ParsecT D u m Word
anyWord = rat _anyWord

-- | Parse a redirection word or number.
redirWord :: Monad m => ParsecT D u m (Maybe Word)
redirWord = rat _redirWord

-- | Parse any operator.
anyOperator :: Monad m => ParsecT D u m String
anyOperator = rat _anyOperator

-- | Parse an assignment.
assign :: Monad m => ParsecT D u m Assign
assign = rat _assign
