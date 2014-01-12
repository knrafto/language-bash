{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Bash conditional commands.
module Language.Bash.Cond
    ( CondExpr(..)
    , parseCondExpr
    , unaryOps
    , binaryOps
    ) where

import Control.Applicative
import Data.Foldable
import Data.Functor.Identity
import Data.Traversable
import Text.Parsec           hiding ((<|>), token, oneOf)
import Text.Parsec.Expr

-- | Bash conditional expressions.
data CondExpr a
    = Unary String a
    | Binary a String a
    | Not (CondExpr a)
    | And (CondExpr a) (CondExpr a)
    | Or (CondExpr a) (CondExpr a)
    deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | A parser over lists of strings.
type Parser = ParsecT [String] () Identity

-- | Parse a primitive token satisfying a predicate.
token :: (String -> Maybe a) -> Parser a
token f = tokenPrim show (\pos _ _ -> pos) f

-- | Parse any word.
anyWord :: Parser String
anyWord = token Just

-- | Parse a given word.
word :: String -> Parser String
word s = token (\t -> if t == s then Just s else Nothing) <?> s

-- | Parse a word in a list.
oneOf :: [String] -> Parser String
oneOf ss = token (\t -> find (== t) ss)

-- | Parse a conditional expression.
-- @condExpr 'True'@ parses for @[[...]]@;
-- @condExpr 'False'@ parses for @test@ (or @[@).
condExpr :: Bool -> Parser (CondExpr String)
condExpr test = expr <* eof
  where
    expr = buildExpressionParser opTable term

    term = word "(" *> expr <* word ")"
       <|> Unary <$> unaryOp <*> anyWord
       <|> (anyWord >>= wordTerm)

    wordTerm w = Binary w <$> binaryOp <*> anyWord
             <|> pure (Unary "-n" w)

    opTable =
        [ [Prefix (Not <$ word "!")          ]
        , [Infix  (And <$ andOp   ) AssocLeft]
        , [Infix  (Or  <$ orOp    ) AssocLeft]
        ]

    andOp    = word (if test then "-a" else "&&")
    orOp     = word (if test then "-o" else "||")
    unaryOp  = oneOf unaryOps  <?> "unary operator"
    binaryOp = oneOf binaryOps <?> "binary operator"

-- | Parse a conditional expression.
-- @parseCondExpr 'True'@ parses for @[[...]]@;
-- @parseCondExpr 'False'@ parses for @test@ (or @[@).
parseCondExpr :: Bool -> [String] -> Either ParseError (CondExpr String)
parseCondExpr test = parse (condExpr test) ""

-- | Unary conditional operators.
unaryOps :: [String]
unaryOps = map (\c -> ['-', c]) "abcdefghknoprstuvwxzGLNOS"

-- | Binary conditional operators.
binaryOps :: [String]
binaryOps = [ "-ef", "-nt", "-ot"
            , "==", "=", "=~", "!=", "<", ">"
            , "-eq", "-ne", "-lt", "-le", "-gt", "-ge"
            ]
