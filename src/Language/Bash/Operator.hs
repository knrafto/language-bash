-- | Bash has a lot of operators.
module Language.Bash.Operator
    ( Operator(..)
    , select
    , selectOperator
    , prettyOperator
    ) where

import Control.Applicative (Alternative)
import Data.Foldable (asum)
import Prettyprinter (Doc, pretty)

-- | String operators.
class Eq a => Operator a where
    operatorTable :: [(a, String)]

-- | Select the first element that succeeds from a table.
select :: Alternative f => (b -> f c) -> [(a, b)] -> f a
select p = asum . map (\(a, b) -> a <$ p b)

-- | Select an operator from the 'String' it represents.
selectOperator :: (Alternative f, Operator a) => (String -> f c) -> f a
selectOperator p = select p operatorTable

-- | Render an operator.
prettyOperator :: Operator a => a -> Doc ann
prettyOperator = pretty . flip lookup operatorTable
