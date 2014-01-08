{-# LANGUAGE OverloadedStrings #-}
-- | Pretty printing of Bash scripts. This tries to stay as close to the format
-- used by @declare -f@ as possible.
module Language.Bash.PrettyPrint
    ( Pretty(..)
    ) where

import Text.PrettyPrint

import Language.Bash.Syntax

-- | Render a type to a pretty-printed 'Doc'.
class Pretty a where
    pretty     :: a -> Doc
    prettyList :: [a] -> Doc
    prettyList = hsep . map pretty

instance Pretty a => Pretty [a] where
    pretty = prettyList

instance Pretty Char where
    pretty c   = text [c]
    prettyList = text

instance Pretty a => Pretty (Maybe a) where
    pretty = maybe empty pretty

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    pretty = either pretty pretty

instance Pretty Command where
    pretty (Command c rs) = pretty c <+> pretty rs

instance Pretty ShellCommand where
    pretty (SimpleCommand as ws)  = pretty as <+> pretty ws
    pretty (AssignBuiltin w args) = text w <+> pretty args
    pretty (FunctionDef name l) =
        text name <+> "()" $+$ pretty (Group l)
    pretty (Coproc name c) =
        "coproc" <+> text name <+> pretty c
    pretty (Subshell l) =
        "(" <+> pretty l <+> ")"
    pretty (Group l) =
        "{" $+$ nest 4 (pretty l) $+$ "}"
    pretty (Arith s) =
        "((" <> text s <> "))"
    pretty (Cond ws) =
        "[[" <+> pretty ws <+> "]]"
    pretty (For w ws l) =
        "for" <+> text w <+> "in" <+> pretty ws <> ";" $+$ doDone l
    pretty (ArithFor s l) =
        "for" <+> "((" <> text s <> "))" $+$ doDone l
    pretty (Select w ws l) =
        "select" <+> text w <+> "in" <+> pretty ws <> ";" $+$ doDone l
    pretty (Case w cs) =
        "case" <+> text w <+> "in" $+$
        nest 4 (pretty cs) $+$
        "esac"
    pretty (If p t f) =
        "if" <+> pretty p <+> "then" $+$
        nest 4 (pretty t) $+$
        maybe empty (\l -> "else" $+$ nest 4 (pretty l)) f $+$
        "fi"
    pretty (Until p l) =
        "until" <+> pretty p <+> doDone l
    pretty (While p l) =
        "while" <+> pretty p <+> doDone l

instance Pretty CaseClause where
    pretty (CaseClause ps l term) =
        hsep (punctuate " | " (map text ps)) <> ")" $+$
        nest 4 (pretty l) $+$
        pretty term

instance Pretty CaseTerm where
    pretty Break       = ";;"
    pretty FallThrough = ";&"
    pretty Continue    = ";;&"

instance Pretty Redir where
    pretty (Redir rword op rhs) =
        pretty rword <> text op <+> text rhs
    pretty (Heredoc op delim quoted doc) =
        text op <> text (if quoted then "'" ++ delim ++ "'" else delim) <>
        "\n" <> text doc <> text delim <> "\n"

    prettyList = foldr f empty
      where
        f a@(Redir{})   b = pretty a <+> b
        f a@(Heredoc{}) b = pretty a <> b

instance Pretty List where
    pretty (List as) = pretty as

instance Pretty Statement where
    pretty (Statement l Sequential)   = pretty l <> ";"
    pretty (Statement l Asynchronous) = pretty l <+> "&"

    prettyList = foldr f empty
      where
        f a@(Statement _ Sequential)   b = pretty a $+$ b
        f a@(Statement _ Asynchronous) b = pretty a <+> b

instance Pretty ListTerm where
    pretty Sequential = ";"
    pretty Asynchronous    = "&"

instance Pretty AndOr where
    pretty (Last p)  = pretty p
    pretty (And p a) = pretty p <+> "&&" <+> pretty a
    pretty (Or p a)  = pretty p <+> "||" <+> pretty a

instance Pretty Pipeline where
    pretty (Time flag p) =
        "time" <+> (if flag then "-p" else empty) <+> pretty p
    pretty (Invert p)    = "!" <+> pretty p
    pretty (Pipeline cs) = hsep . punctuate " | " $ map pretty cs

instance Pretty Assign where
    pretty (Assign lhs op rhs) = pretty lhs <> pretty op <> pretty rhs

instance Pretty LValue where
    pretty (LValue name sub) = text name <> pretty sub

instance Pretty Subscript where
    pretty (Subscript Nothing)  = empty
    pretty (Subscript (Just w)) = "[" <> text w <> "]"

instance Pretty AssignOp where
    pretty Equals     = "="
    pretty PlusEquals = "+="

instance Pretty RValue where
    pretty (RValue w)  = text w
    pretty (RArray rs) = "(" <> hsep (map f rs) <> ")"
      where
        f (sub, w) = pretty sub <> "=" <> text w

-- | Render a @do...done@ block.
doDone :: Pretty a => a -> Doc
doDone a = "do" $+$ nest 4 (pretty a) $+$ "done"
