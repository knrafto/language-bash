{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PatternGuards
  , RecordWildCards
  #-}
-- | Memoized packrat parsing, inspired by Edward Kmett\'s
-- \"A Parsec Full of Rats\".
module Language.Bash.Parse.Internal
    ( -- * Packrat parsing
      D
    , pack
      -- * Tokens
    , satisfying
      -- * Whitespace
    , I.skipSpace
      -- * Words
    , anyWord
    , word
    , reservedWord
    , unreservedWord
    , assignBuiltin
    , ioDesc
    , name
      -- * Operators
    , anyOperator
    , operator
      -- * Assignments
    , assign
      -- * Arithmetic expressions
    , arith
      -- * Here documents
    , heredocWord
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Function
import           Data.Functor.Identity
import           Text.Parsec.Char
import           Text.Parsec.Combinator   hiding (optional)
import           Text.Parsec.Error
import           Text.Parsec.Prim         hiding ((<|>), token)
import           Text.Parsec.Pos

import qualified Language.Bash.Parse.Word as I
import           Language.Bash.Pretty
import           Language.Bash.Syntax
import           Language.Bash.Word

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

-- | Run a parser, merging it with another.
reparse :: Stream s m t0 => ParsecT s u m a -> s -> ParsecT t u m a
reparse p input = mkPT $ \s0@(State _ _ u) ->
    (fmap return . patch s0) `liftM` runParserT p u "" input
  where
    patch (State _ pos _) (Left e)  = Empty (Error (setErrorPos pos e))
    patch s               (Right r) = Empty (Ok r s (unknownError s))

-- | A token.
data Token
    = TWord Word
    | TIODesc IODesc

-- | A stream with memoized results.
data D = D
    { _token       :: Result D Token
    , _anyWord     :: Result D Word
    , _ioDesc      :: Result D IODesc
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
            t <- I.word
            guard $ not (null t)
            next <- optional (lookAhead anyChar)
            I.skipSpace
            return $ case next of
                Just c | c == '<' || c == '>'
                       , Right desc <- parse (descriptor <* eof) "" (prettyText t)
                  -> TIODesc desc
                _ -> TWord t
        _anyWord     = result $ token >>= \case
            TWord w -> return w
            _       -> empty
        _ioDesc      = result $ token >>= \case
            TIODesc desc -> return desc
            _            -> empty
        _anyOperator = result $ I.operator operators <* I.skipSpace
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
satisfying a p = try $ do
    t <- a
    if p t then return t else unexpected (show t)

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

-- | All Bash operators.
operators :: [String]
operators =
    [ "(", ")", ";;", ";&", ";;&"
    , "|", "|&", "||", "&&", ";", "&", "\n"
    , "<", ">", ">|", ">>", "&>", "&>>", "<<<", "<&", ">&", "<>"
    , "<<", "<<-"
    ]

-- | Parse a descriptor.
descriptor :: Stream s m Char => ParsecT s u m IODesc
descriptor = IONumber . read <$> many1 digit
         <|> IOVar <$ char '{' <*> I.name <* char '}'

-- | Parse a single token.
token :: Monad m => ParsecT D u m Token
token = try (rat _token) <?> "token"

-- | Parse any word.
anyWord :: Monad m => ParsecT D u m Word
anyWord = try (rat _anyWord) <?> "word"

-- | Parse the given word.
word :: Monad m => Word -> ParsecT D u m Word
word w = anyWord `satisfying` (== w) <?> prettyText w

-- | Parse a reversed word.
reservedWord :: Monad m => ParsecT D u m Word
reservedWord = anyWord `satisfying` (`elem` reservedWords) <?> "reserved word"

-- | Parse a word that is not reserved.
unreservedWord :: Monad m => ParsecT D u m Word
unreservedWord = anyWord `satisfying` (`notElem` reservedWords)
    <?> "unreserved word"

-- | Parse an assignment builtin.
assignBuiltin :: Monad m => ParsecT D u m Word
assignBuiltin = anyWord `satisfying` (`elem` assignBuiltins)
    <?> "assignment builtin"

-- | Parse a redirection word or number.
ioDesc :: Monad m => ParsecT D u m IODesc
ioDesc = try (rat _ioDesc) <?> "IO descriptor"

-- | Parse a variable name.
name :: Monad m => ParsecT D u m String
name = (prettyText <$> unreservedWord) `satisfying` isName <?> "name"
  where
    isName s = case parse (I.name <* eof) "" (prettyText s) of
        Left _  -> False
        Right _ -> True

-- | Parse any operator.
anyOperator :: Monad m => ParsecT D u m String
anyOperator = try (rat _anyOperator) <?> "operator"

-- | Parse a given operator.
operator :: Monad m => String -> ParsecT D u m String
operator op = anyOperator `satisfying` (== op) <?> op

-- | Parse an assignment.
assign :: Monad m => ParsecT D u m Assign
assign = try (rat _assign) <?> "assignment"

-- | Parse an arithmetic expression.
arith :: Monad m => ParsecT D u m String
arith = try (string "((") *> I.arith <* string "))" <* I.skipSpace
    <?> "arithmetic expression"

-- | Reparse a here document into a word.
heredocWord :: Monad m => String -> ParsecT s u m Word
heredocWord = reparse I.heredocWord
