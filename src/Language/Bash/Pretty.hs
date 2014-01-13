-- | Pretty-printing of Bash scripts. This tries to stay close to the format
-- used by the Bash builtin @declare -f@.
module Language.Bash.Pretty
    ( Pretty(..)
    , prettyText
    ) where

import Text.PrettyPrint

-- | A class of types which may be pretty-printed.
class Pretty a where
    -- | Pretty-print to a 'Doc'.
    pretty     :: a -> Doc

    -- | Pretty-print a list. By default, this separates each element with
    -- a space using 'hsep'.
    prettyList :: [a] -> Doc
    prettyList = hsep . map pretty

instance Pretty a => Pretty [a] where
    pretty = prettyList

instance Pretty Doc where
    pretty = id

instance Pretty Char where
    pretty c   = text [c]
    prettyList = text

instance Pretty a => Pretty (Maybe a) where
    pretty = maybe empty pretty

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    pretty = either pretty pretty

-- | Pretty-print to a 'String'.
prettyText :: Pretty a => a -> String
prettyText = render . pretty
