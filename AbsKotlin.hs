-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module AbsKotlin where

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)

data Prog = Program [Inst]
  deriving (Eq, Ord, Show, Read)

data Exp
    = Eassign Exp OpAssign Exp
    | Eternary Exp Exp Exp
    | Eor Exp Exp
    | Eand Exp Exp
    | Eeq Exp Exp
    | Eneq Exp Exp
    | El Exp Exp
    | Eg Exp Exp
    | Ele Exp Exp
    | Ege Exp Exp
    | Eadd Exp Exp
    | Esub Exp Exp
    | Emul Exp Exp
    | Ediv Exp Exp
    | Emod Exp Exp
    | Eneg Exp
    | Elneg Exp
    | Einc Exp
    | Edec Exp
    | EPinc Exp
    | EPdec Exp
    | Eiter Iterable
    | Earray Exp Exp
    | Etupla [Exp]
    | Eint Integer
    | Estring String
    | Etrue
    | Efalse
    | Enull
    | Ecall FunctionExp
    | Eget Ident [DimExp]
    | Elambda [Arg] [Stm]
    | Ennass Exp
    | Evar Ident
  deriving (Eq, Ord, Show, Read)

data DimExp = Dim Exp
  deriving (Eq, Ord, Show, Read)

data OpAssign
    = OpAssign1
    | OpAssign2
    | OpAssign3
    | OpAssign4
    | OpAssign5
    | OpAssign6
  deriving (Eq, Ord, Show, Read)

data BaseType
    = Ttupla [Type] | Tbool | Tint | Tstring | Tarray Type
  deriving (Eq, Ord, Show, Read)

data RetType = TRunit | TRtype Type
  deriving (Eq, Ord, Show, Read)

data Type
    = Tnullable BaseType | Tnonnull BaseType | Tfun [Type] RetType | Tnull | Help Integer | HelpRet RetType | Tunit
  deriving (Eq, Ord, Show, Read)

data Arg = Args Ident Type
  deriving (Eq, Ord, Show, Read)

data Stm
    = Sdec Dec
    | Sexp Exp
    | Sblock [Stm]
    | Sfor Ident Exp [Stm]
    | Swhile Exp [Stm]
    | Sbreak
    | Scont
    | Sretexp Exp
    | Sret
    | Sif Exp [Stm]
    | Sifelse Exp [Stm] [Stm]
    | Sprint Exp
    | Sprintln Exp
    | Snotnull Exp [Stm]
  deriving (Eq, Ord, Show, Read)

data Inst = Idec Dec
  deriving (Eq, Ord, Show, Read)

data Dec
    = Dfun FunctionDec
    | Dvar Ident Type Exp
    | Dval Ident Type Exp
    | Dvarnull Ident Type
    | Dvalnull Ident Type
  deriving (Eq, Ord, Show, Read)

data FunctionDec = FunDec Ident [Arg] RetType [Stm]
  deriving (Eq, Ord, Show, Read)

data FunctionExp = FunCall Ident [Exp]
  deriving (Eq, Ord, Show, Read)

data Iterable
    = Itrange Exp Exp
    | Itup Exp Exp
    | Itdown Exp Exp
    | Itupst Exp Exp Exp
    | Itdownst Exp Exp Exp
  deriving (Eq, Ord, Show, Read)

