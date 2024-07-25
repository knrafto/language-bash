{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFoldable
  , DeriveGeneric
  , DeriveFunctor
  , DeriveTraversable
  , OverloadedStrings
  #-}
-- | Bash conditional commands.
module Language.Bash.Cond
    ( CondExpr(..)
    , UnaryOp(..)
    , BinaryOp(..)
    , parseTestExpr
    ) where

import Prelude                hiding (negate)

import Control.Applicative
import Data.Data              (Data)
import Data.Typeable          (Typeable)
import GHC.Generics           (Generic)
import Text.Parsec            hiding ((<|>), token)
import Text.Parsec.Expr       hiding (Operator)
import Prettyprinter          (Pretty(..), (<+>))

import Language.Bash.Operator

-- | Bash conditional expressions.
data CondExpr a
    = Unary UnaryOp a
    | Binary a BinaryOp a
    | Not (CondExpr a)
    | And (CondExpr a) (CondExpr a)
    | Or (CondExpr a) (CondExpr a)
    deriving (Data, Eq, Read, Show, Typeable, Functor, Foldable, Traversable, Generic)

instance Pretty a => Pretty (CondExpr a) where
    pretty = go (0 :: Int)
      where
        go _ (Unary op a)    = pretty op <+> pretty a
        go _ (Binary a op b) = pretty a <+> pretty op <+> pretty b
        go _ (Not e)         = "!" <+> go 2 e
        go p (And e1 e2)     = paren (p > 1) $ go 1 e1 <+> "&&" <+> go 1 e2
        go p (Or e1 e2)      = paren (p > 0) $ go 0 e1 <+> "||" <+> go 0 e2

        paren False d = d
        paren True d  = "(" <+> d <+> ")"

-- | Unary conditional operators.
data UnaryOp
    = BlockFile      -- ^ @-b@
    | CharacterFile  -- ^ @-c@
    | Directory      -- ^ @-d@
    | FileExists     -- ^ @-e@, @-a@
    | RegularFile    -- ^ @-f@
    | SetGID         -- ^ @-g@
    | Sticky         -- ^ @-k@
    | NamedPipe      -- ^ @-p@
    | Readable       -- ^ @-r@
    | FileSize       -- ^ @-s@
    | Terminal       -- ^ @-t@
    | SetUID         -- ^ @-u@
    | Writable       -- ^ @-w@
    | Executable     -- ^ @-x@
    | GroupOwned     -- ^ @-G@
    | SymbolicLink   -- ^ @-L@, @-h@
    | Modified       -- ^ @-N@
    | UserOwned      -- ^ @-O@
    | Socket         -- ^ @-S@
    | Optname        -- ^ @-o@
    | Varname        -- ^ @-v@
    | ZeroString     -- ^ @-z@
    | NonzeroString  -- ^ @-n /string/@ or @/string/@
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded, Generic)

instance Operator UnaryOp where
    operatorTable =
        zip [minBound .. maxBound]
            (map (\c -> ['-', c]) "bcdefgkprstuwxGLNOSovzn") ++
        [ (FileExists  , "-a")
        , (SymbolicLink, "-h")
        ]

instance Pretty UnaryOp where
    pretty = prettyOperator

-- | Binary conditional operators.
data BinaryOp
    = SameFile   -- ^ @-ef@
    | NewerThan  -- ^ @-nt@
    | OlderThan  -- ^ @-ot@
    | StrMatch   -- ^ @=~@
    | StrEQ      -- ^ @==@, @=@
    | StrNE      -- ^ @!=@
    | StrLT      -- ^ @<@
    | StrGT      -- ^ @>@
    | ArithEQ    -- ^ @-eq@
    | ArithNE    -- ^ @-ne@
    | ArithLT    -- ^ @-lt@
    | ArithLE    -- ^ @-le@
    | ArithGT    -- ^ @-gt@
    | ArithGE    -- ^ @-ge@
    deriving (Data, Eq, Ord, Read, Show, Typeable, Enum, Bounded, Generic)

instance Operator BinaryOp where
    operatorTable =
        zip [minBound .. maxBound]
            [ "-ef", "-nt", "-ot"
            , "=~", "==", "!=", "<", ">"
            , "-eq", "-ne", "-lt", "-le", "-gt", "-ge"
            ] ++
        [ (StrEQ, "=") ]

instance Pretty BinaryOp where
    pretty = prettyOperator

-- | A parser over lists of strings.
type Parser = Parsec [String] ()

-- | Parse a token.
token :: (String -> Maybe a) -> Parser a
token = tokenPrim show (\pos _ _ -> pos)

-- | Parse a specific word.
word :: String -> Parser String
word s = token (\t -> if t == s then Just s else Nothing) <?> s

-- | Parse any word.
anyWord :: Parser String
anyWord = token Just

-- | Parse in parentheses.
parens :: Parser a -> Parser a
parens p = word "(" *> p <* word ")"

-- | Parse a nullary expression
nullaryExpr :: Parser (CondExpr String)
nullaryExpr = Unary NonzeroString <$> anyWord

-- | Parse a unary expression.
unaryExpr :: Parser (CondExpr String)
unaryExpr = Unary <$> select word unaryOps <*> anyWord
        <?> "unary expression"
  where
    unaryOps = filter ((`notElem` ["-a", "-o"]) . snd) operatorTable

-- | Parse a standalone unary expression.
standaloneUnaryExpr :: Parser (CondExpr String)
standaloneUnaryExpr = Unary <$> selectOperator word <*> anyWord
                  <?> "unary expression"

-- | Parse a binary expression.
binaryExpr :: Parser (CondExpr String)
binaryExpr = Binary <$> anyWord <*> select word binaryOps <*> anyWord
         <?> "binary expression"
  where
    binaryOps = filter ((/= "=~") . snd) operatorTable

-- | Parse a binary @-a@ or @-o@ expression.
binaryAndOrExpr :: Parser (CondExpr String)
binaryAndOrExpr = nullaryExpr <**> andOrOp <*> nullaryExpr
  where
    andOrOp = And <$ word "-a"
          <|> Or  <$ word "-o"

-- | Parse a conditional expression.
condExpr :: Parser (CondExpr String)
condExpr = expr
  where
    expr = buildExpressionParser opTable term

    term = parens expr
       <|> unaryExpr
       <|> try binaryExpr
       <|> nullaryExpr

    opTable =
        [ [Prefix (Not <$ word "!")]
        , [Infix  (And <$ word "-a") AssocLeft]
        , [Infix  (Or  <$ word "-o") AssocLeft]
        ]

-- | Parse a conditional expression for the Bash @test@ builtin.
parseTestExpr :: [String] -> Either ParseError (CondExpr String)
parseTestExpr args = parse (testExpr <* eof) "" args
  where
    testExpr = case length args of
        0 -> fail "no arguments"
        1 -> oneArg
        2 -> twoArg
        3 -> threeArg
        4 -> fourArg
        _ -> condExpr

    oneArg = nullaryExpr

    twoArg = negate nullaryExpr
         <|> standaloneUnaryExpr

    threeArg = try binaryExpr
           <|> try binaryAndOrExpr
           <|> negate twoArg
           <|> parens nullaryExpr

    fourArg = negate threeArg
          <|> condExpr

    negate p = Not <$ word "!" <*> p
