-- | Bash has a lot of operators.
module Language.Bash.Operator
    ( Operator(..)
    , select
    , selectOperator
    , prettyOperator
    ) where

import Control.Applicative
import Text.PrettyPrint     hiding (empty)

import Language.Bash.Pretty

-- | String operators.
class Eq a => Operator a where
    operatorTable :: [(a, String)]

-- | Select the first element that succeeds from a table.
select :: Alternative f => (b -> f c) -> [(a, b)] -> f a
select p = foldr (<|>) empty . map (\(a, b) -> a <$ p b)

-- | Select an operator from the 'String' it represents.
selectOperator :: (Alternative f, Operator a) => (String -> f c) -> f a
selectOperator p = select p operatorTable

-- | Render an operator.
prettyOperator :: Operator a => a -> Doc
prettyOperator = pretty . flip lookup operatorTable
