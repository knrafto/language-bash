-- | Pretty-printing of Bash scripts. This tries to stay close to the format
-- used by the Bash builtin @declare -f@.
module Language.Bash.Pretty where

import Prettyprinter
import Prettyprinter.Internal.Type (Doc(Empty))
import Prettyprinter.Render.String (renderString)

-- | @x $+$ y@ concatenates @x@ and @y@ with a 'line' in between
($+$) :: Doc ann -> Doc ann -> Doc ann
x $+$ y = x <> line <> y

-- | Behaves like '$+$' except that if one of the documents was empty we do not concatenate at all.
-- 'mempty' is the identity of '$+$':
--
-- prop> x $++$ mempty == x
--
-- and
--
-- prop> mempty $++$ y == y
($++$) :: Doc ann -> Doc ann -> Doc ann
Empty $++$ y     = y
x     $++$ Empty = x
x     $++$ y     = x <> line <> y

-- | Behaves like '<+>' except that if one of the documents was empty we do not concatenate at all.
-- 'mempty' is the identity of '<+>':
--
-- prop> x <++> mempty == x
--
-- and
--
-- prop> mempty <++> y == y
(<++>) :: Doc ann -> Doc ann -> Doc ann
Empty <++> y     = y
x     <++> Empty = x
x     <++> y     = x <+> y

-- | Pretty-print to a 'String'.
prettyText :: Pretty a => a -> String
prettyText = renderString . layoutPretty defaultLayoutOptions . pretty
