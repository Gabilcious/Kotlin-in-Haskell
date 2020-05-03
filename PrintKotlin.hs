{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintKotlin.
--   Generated by the BNF converter.

module PrintKotlin where

import qualified AbsKotlin
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsKotlin.Ident where
  prt _ (AbsKotlin.Ident i) = doc (showString i)

instance Print AbsKotlin.Prog where
  prt i e = case e of
    AbsKotlin.Program insts -> prPrec i 0 (concatD [prt 0 insts])

instance Print AbsKotlin.Exp where
  prt i e = case e of
    AbsKotlin.Eassign exp1 opassign exp2 -> prPrec i 0 (concatD [prt 0 exp1, prt 0 opassign, prt 1 exp2])
    AbsKotlin.Eternary exp1 exp2 exp3 -> prPrec i 1 (concatD [prt 3 exp1, doc (showString "?"), prt 1 exp2, doc (showString ":"), prt 2 exp3])
    AbsKotlin.Eor exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "||"), prt 3 exp2])
    AbsKotlin.Eand exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "&&"), prt 4 exp2])
    AbsKotlin.Eeq exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "=="), prt 5 exp2])
    AbsKotlin.Eneq exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "!="), prt 5 exp2])
    AbsKotlin.El exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "<"), prt 6 exp2])
    AbsKotlin.Eg exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString ">"), prt 6 exp2])
    AbsKotlin.Ele exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "<="), prt 6 exp2])
    AbsKotlin.Ege exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString ">="), prt 6 exp2])
    AbsKotlin.Eadd exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "+"), prt 7 exp2])
    AbsKotlin.Esub exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "-"), prt 7 exp2])
    AbsKotlin.Emul exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "*"), prt 8 exp2])
    AbsKotlin.Ediv exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "/"), prt 8 exp2])
    AbsKotlin.Emod exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "%"), prt 8 exp2])
    AbsKotlin.Eneg exp -> prPrec i 8 (concatD [doc (showString "-"), prt 8 exp])
    AbsKotlin.Elneg exp -> prPrec i 8 (concatD [doc (showString "!"), prt 8 exp])
    AbsKotlin.Einc exp -> prPrec i 8 (concatD [doc (showString "++"), prt 11 exp])
    AbsKotlin.Edec exp -> prPrec i 8 (concatD [doc (showString "--"), prt 11 exp])
    AbsKotlin.EPinc exp -> prPrec i 9 (concatD [prt 11 exp, doc (showString "++")])
    AbsKotlin.EPdec exp -> prPrec i 9 (concatD [prt 11 exp, doc (showString "--")])
    AbsKotlin.Etupla exps -> prPrec i 10 (concatD [doc (showString "Tupla"), doc (showString "("), prt 0 exps, doc (showString ")")])
    AbsKotlin.Eint n -> prPrec i 10 (concatD [prt 0 n])
    AbsKotlin.Estring str -> prPrec i 10 (concatD [prt 0 str])
    AbsKotlin.Etrue -> prPrec i 10 (concatD [doc (showString "true")])
    AbsKotlin.Efalse -> prPrec i 10 (concatD [doc (showString "false")])
    AbsKotlin.Enull -> prPrec i 10 (concatD [doc (showString "null")])
    AbsKotlin.Ecall functionexp -> prPrec i 10 (concatD [prt 0 functionexp])
    AbsKotlin.Eget id dimexps -> prPrec i 10 (concatD [prt 0 id, prt 0 dimexps])
    AbsKotlin.Elambda args stms -> prPrec i 10 (concatD [doc (showString "{"), prt 0 args, doc (showString "->"), prt 0 stms, doc (showString "}")])
    AbsKotlin.Ennass exp -> prPrec i 11 (concatD [prt 11 exp, doc (showString "!!")])
    AbsKotlin.Evar id -> prPrec i 12 (concatD [prt 0 id])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsKotlin.Exp] where
  prt = prtList

instance Print AbsKotlin.DimExp where
  prt i e = case e of
    AbsKotlin.Dim exp -> prPrec i 0 (concatD [doc (showString "["), prt 0 exp, doc (showString "]")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsKotlin.DimExp] where
  prt = prtList

instance Print AbsKotlin.OpAssign where
  prt i e = case e of
    AbsKotlin.OpAssign1 -> prPrec i 0 (concatD [doc (showString "=")])
    AbsKotlin.OpAssign2 -> prPrec i 0 (concatD [doc (showString "+=")])
    AbsKotlin.OpAssign3 -> prPrec i 0 (concatD [doc (showString "-=")])
    AbsKotlin.OpAssign4 -> prPrec i 0 (concatD [doc (showString "*=")])
    AbsKotlin.OpAssign5 -> prPrec i 0 (concatD [doc (showString "/=")])
    AbsKotlin.OpAssign6 -> prPrec i 0 (concatD [doc (showString "%=")])

instance Print AbsKotlin.BaseType where
  prt i e = case e of
    AbsKotlin.Ttupla types -> prPrec i 0 (concatD [doc (showString "Tupla"), doc (showString "<"), prt 0 types, doc (showString ">")])
    AbsKotlin.Tbool -> prPrec i 0 (concatD [doc (showString "Bool")])
    AbsKotlin.Tint -> prPrec i 0 (concatD [doc (showString "Int")])
    AbsKotlin.Tstring -> prPrec i 0 (concatD [doc (showString "String")])

instance Print AbsKotlin.Type where
  prt i e = case e of
    AbsKotlin.Tunit -> prPrec i 0 (concatD [doc (showString "Unit")])
    AbsKotlin.Tnull basetype -> prPrec i 0 (concatD [prt 0 basetype, doc (showString "?")])
    AbsKotlin.Tnonnull basetype -> prPrec i 0 (concatD [prt 0 basetype])
    AbsKotlin.Tfun types basetype -> prPrec i 0 (concatD [doc (showString "("), prt 0 types, doc (showString ")"), doc (showString "->"), prt 0 basetype])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsKotlin.Type] where
  prt = prtList

instance Print AbsKotlin.Arg where
  prt i e = case e of
    AbsKotlin.Args id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 type_])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsKotlin.Arg] where
  prt = prtList

instance Print AbsKotlin.Stm where
  prt i e = case e of
    AbsKotlin.Sdec dec -> prPrec i 0 (concatD [prt 0 dec])
    AbsKotlin.Sexp exp -> prPrec i 0 (concatD [prt 0 exp, doc (showString ";")])
    AbsKotlin.Sblock stms -> prPrec i 0 (concatD [doc (showString "run"), doc (showString "{"), prt 0 stms, doc (showString "}")])
    AbsKotlin.Sfor id iterable stms -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 id, doc (showString "in"), prt 0 iterable, doc (showString ")"), doc (showString "{"), prt 0 stms, doc (showString "}")])
    AbsKotlin.Swhile exp stms -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString "{"), prt 0 stms, doc (showString "}")])
    AbsKotlin.Sbreak -> prPrec i 0 (concatD [doc (showString "break"), doc (showString ";")])
    AbsKotlin.Scont -> prPrec i 0 (concatD [doc (showString "continue"), doc (showString ";")])
    AbsKotlin.Sretexp exp -> prPrec i 0 (concatD [doc (showString "return"), prt 0 exp, doc (showString ";")])
    AbsKotlin.Sret -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    AbsKotlin.Sif exp stms -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString "{"), prt 0 stms, doc (showString "}")])
    AbsKotlin.Sifelse exp stms1 stms2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString "{"), prt 0 stms1, doc (showString "}"), doc (showString "else"), doc (showString "{"), prt 0 stms2, doc (showString "}")])
    AbsKotlin.Sprint exp -> prPrec i 0 (concatD [doc (showString "print"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString ";")])
    AbsKotlin.Sprintln exp -> prPrec i 0 (concatD [doc (showString "println"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString ";")])
    AbsKotlin.Snotnull exp stms -> prPrec i 0 (concatD [prt 0 exp, doc (showString "?.let"), doc (showString "{"), prt 0 stms, doc (showString "}")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsKotlin.Stm] where
  prt = prtList

instance Print AbsKotlin.Inst where
  prt i e = case e of
    AbsKotlin.Idec dec -> prPrec i 0 (concatD [prt 0 dec])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsKotlin.Inst] where
  prt = prtList

instance Print AbsKotlin.Dec where
  prt i e = case e of
    AbsKotlin.Dfun functiondec -> prPrec i 0 (concatD [prt 0 functiondec])
    AbsKotlin.Darray arraydec -> prPrec i 0 (concatD [prt 0 arraydec])
    AbsKotlin.Dvar id type_ exp -> prPrec i 0 (concatD [doc (showString "var"), prt 0 id, doc (showString ":"), prt 0 type_, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsKotlin.Dval id type_ exp -> prPrec i 0 (concatD [doc (showString "val"), prt 0 id, doc (showString ":"), prt 0 type_, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsKotlin.Dvarnull id type_ -> prPrec i 0 (concatD [doc (showString "var"), prt 0 id, doc (showString ":"), prt 0 type_, doc (showString ";")])
    AbsKotlin.Dvalnull id type_ -> prPrec i 0 (concatD [doc (showString "val"), prt 0 id, doc (showString ":"), prt 0 type_, doc (showString ";")])

instance Print AbsKotlin.FunctionDec where
  prt i e = case e of
    AbsKotlin.FunDec id args type_ stms -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 id, doc (showString "("), prt 0 args, doc (showString ")"), doc (showString ":"), prt 0 type_, doc (showString "{"), prt 0 stms, doc (showString "}")])

instance Print AbsKotlin.FunctionExp where
  prt i e = case e of
    AbsKotlin.FunCall id exps -> prPrec i 0 (concatD [prt 0 id, doc (showString "("), prt 0 exps, doc (showString ")")])

instance Print AbsKotlin.Iterable where
  prt i e = case e of
    AbsKotlin.Itarray id -> prPrec i 0 (concatD [prt 0 id])
    AbsKotlin.Itrange exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString ".."), prt 0 exp2])
    AbsKotlin.Itup exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "until"), prt 0 exp2])
    AbsKotlin.Itdown exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "downTo"), prt 0 exp2])
    AbsKotlin.Itupst exp1 exp2 exp3 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "until"), prt 0 exp2, doc (showString "step"), prt 0 exp3])
    AbsKotlin.Itdownst exp1 exp2 exp3 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "downTo"), prt 0 exp2, doc (showString "step"), prt 0 exp3])

instance Print AbsKotlin.ArrayDec where
  prt i e = case e of
    AbsKotlin.ArrDec id exp1 exp2 -> prPrec i 0 (concatD [doc (showString "val"), prt 0 id, doc (showString "="), doc (showString "Array"), doc (showString "("), prt 0 exp1, doc (showString ","), prt 0 exp2, doc (showString ")"), doc (showString ";")])
    AbsKotlin.ArrItDec id iterable -> prPrec i 0 (concatD [doc (showString "val"), prt 0 id, doc (showString "="), doc (showString "["), prt 0 iterable, doc (showString "]"), doc (showString ";")])

