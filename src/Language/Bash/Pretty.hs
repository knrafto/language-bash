{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | Pretty-printing of Bash scripts. This tries to stay close to the format
-- used by the Bash builtin @declare -f@.
module Language.Bash.Pretty
    ( Pretty(..)
    , render
    ) where

import Text.PrettyPrint

import Language.Bash.Syntax

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

instance Pretty Command where
    pretty (Command c rs) = pretty c <+> pretty rs

instance Pretty Redir where
    pretty Redir{..} =
        pretty redirDesc <> text redirOp <> text redirTarget
    pretty Heredoc{..} =
        text redirOp <>
        text (if heredocDelimQuoted
              then "'" ++ heredocDelim ++ "'"
              else heredocDelim) <> "\n" <>
        text document <> text heredocDelim <> "\n"

    prettyList = foldr f empty
      where
        f a@(Redir{})   b = pretty a <+> b
        f a@(Heredoc{}) b = pretty a <> b

instance Pretty IODesc where
    pretty (IONumber n) = int n
    pretty (IOVar n)    = "{" <> text n <> "}"

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
        "{" $+$ indent l $+$ "}"
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
        "case" <+> text w <+> "in" $+$ indent cs $+$ "esac"
    pretty (If p t f) =
        "if" <+> pretty p <+> "then" $+$ indent t $+$
        pretty (fmap (\l -> "else" $+$ indent l) f) $+$
        "fi"
    pretty (Until p l) =
        "until" <+> pretty p <+> doDone l
    pretty (While p l) =
        "while" <+> pretty p <+> doDone l

instance Pretty CaseClause where
    pretty (CaseClause ps l term) =
        hcat (punctuate " | " (map text ps)) <> ")" $+$
        indent l $+$
        pretty term

instance Pretty CaseTerm where
    pretty Break       = ";;"
    pretty FallThrough = ";&"
    pretty Continue    = ";;&"

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
    pretty Sequential   = ";"
    pretty Asynchronous = "&"

instance Pretty AndOr where
    pretty (Last p)  = pretty p
    pretty (And p a) = pretty p <+> "&&" <+> pretty a
    pretty (Or p a)  = pretty p <+> "||" <+> pretty a

instance Pretty Pipeline where
    pretty Pipeline{..} =
        (if timed      then "time" else empty) <+>
        (if timedPosix then "-p"   else empty) <+>
        (if inverted   then "!"    else empty) <+>
        pretty commands

instance Pretty Assign where
    pretty (Assign lhs op rhs) = pretty lhs <> pretty op <> pretty rhs

instance Pretty LValue where
    pretty (LValue name sub) =
        text name <> pretty (fmap (\s -> "[" ++ s ++ "]") sub)

instance Pretty AssignOp where
    pretty Equals     = "="
    pretty PlusEquals = "+="

instance Pretty RValue where
    pretty (RValue w)  = text w
    pretty (RArray rs) = "(" <> hsep (map f rs) <> ")"
      where
        f (sub, w) = pretty (fmap (\s -> "[" ++ s ++ "]=") sub) <> text w

-- | Indent by 4 columns.
indent :: Pretty a => a -> Doc
indent = nest 4 . pretty

-- | Render a @do...done@ block.
doDone :: Pretty a => a -> Doc
doDone a = "do" $+$ indent a $+$ "done"
