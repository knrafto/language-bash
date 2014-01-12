{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , OverloadedStrings
  #-}
-- | Bash conditional commands.
module Language.Bash.Cond
    ( CondExpr(..)
    , unaryOps
    , binaryOps
    ) where

import Data.Foldable
import Data.Traversable
import Text.PrettyPrint

import Language.Bash.Pretty

-- | Bash conditional expressions.
data CondExpr a
    = Unary String a
    | Binary a String a
    | Not (CondExpr a)
    | And (CondExpr a) (CondExpr a)
    | Or (CondExpr a) (CondExpr a)
    deriving (Eq, Read, Show, Functor, Foldable, Traversable)

instance Pretty a => Pretty (CondExpr a) where
    pretty = go (0 :: Int)
      where
        go _ (Unary op a)    = text op <+> pretty a
        go _ (Binary a op b) = pretty a <+> text op <+> pretty b
        go _ (Not e)         = "!" <+> go 2 e
        go p (And e1 e2)     = paren (p > 1) $ go 1 e1 <+> "&&" <+> go 1 e2
        go p (Or e1 e2)      = paren (p > 0) $ go 0 e1 <+> "||" <+> go 0 e2

        paren False d = d
        paren True d  = "(" <+> d <+> ")"

-- | Unary conditional operators.
unaryOps :: [String]
unaryOps = map (\c -> ['-', c]) "abcdefghknoprstuvwxzGLNOS"

-- | Binary conditional operators.
binaryOps :: [String]
binaryOps = [ "-ef", "-nt", "-ot"
            , "==", "=", "=~", "!=", "<", ">"
            , "-eq", "-ne", "-lt", "-le", "-gt", "-ge"
            ]
