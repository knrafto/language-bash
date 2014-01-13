{-# LANGUAGE OverloadedStrings #-}
-- | Bash conditional commands.
module Language.Bash.Cond
    ( CondExpr(..)
    , UnaryOp(..)
    , BinaryOp(..)
    ) where

import Text.PrettyPrint

import Language.Bash.Pretty

-- | Bash conditional expressions.
data CondExpr
    = Unary UnaryOp String
    | Binary String BinaryOp String
    | Not CondExpr
    | And CondExpr CondExpr
    | Or CondExpr CondExpr
    deriving (Eq, Read, Show)

instance Pretty CondExpr where
    pretty = go (0 :: Int)
      where
        go _ (Unary op a)    = pretty op <+> text a
        go _ (Binary a op b) = text a <+> pretty op <+> text b
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
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Pretty UnaryOp where
    pretty op = text ['-', opNames !! fromEnum op]
      where
        opNames = "bcdefgkprstuwxGLNOSovzn"

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
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Pretty BinaryOp where
    pretty op = opNames !! fromEnum op
      where
        opNames = [ "-ef", "-nt", "-ot"
                  , "=~", "==", "!=", "<", ">"
                  , "-eq", "-ne", "-lt", "-le", "-gt", "-ge"
                  ]
