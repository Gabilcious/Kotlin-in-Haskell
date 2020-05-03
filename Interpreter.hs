module Interpreter where

-- Haskell module generated by the BNF converter

import AbsKotlin
import ErrM
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexKotlin
import ParKotlin
import PrintKotlin
import AbsKotlin

import Data.Map
import Data.Map.Internal.Debug
type Result = Err String

-- ------------- --
-- -- D A T A -- --
-- ------------- --

data Value
    = VNull
    | VUnit
    | VInt Integer
    | VString String
    | VBool Bool
    | VTupla [Value]
    | VArray [Value]
    | VFun [Arg] [Stm] Env
  deriving (Eq, Ord, Show, Read)

data Env = E (Map Ident (Integer, Bool))
  deriving (Eq, Ord, Show, Read)

data State = S (Map Integer Value)
  deriving (Eq, Ord, Show, Read)

failure x = error ("Undefined case: " ++ show x)




-- ------------- --
-- P R O G R A M --
-- ------------- --

doMain :: [Stm] -> Env -> State -> IO(Env, State)
doMain stms e s = case stms of
  [] -> return(e,s)
  h:t -> do
      (ne, ns, nv, isRet) <- transStm h e s
      doMain t ne ns

start :: Prog -> IO()
start prog = transProg prog (E empty) (S (singleton 0 (VInt 1)))

transProg :: Prog -> Env -> State -> IO()
transProg x (E em) (S sm) = case x of
  Program [] -> do
      putStrLn ""
      putStrLn ""
      putStrLn ""
      putStrLn (Data.Map.Internal.Debug.showTree em)
      putStrLn (Data.Map.Internal.Debug.showTree sm)
  Program (h:t) -> do
      (ne, ns) <- transInst h (E em) (S sm)
      transProg (Program t) ne ns

transInst :: Inst -> Env -> State -> IO(Env, State)
transInst x = case x of
  Idec dec -> transDec dec

doFun :: [Stm] -> Env -> State -> IO(Env, State, Value)
doFun stms e s = do
    (ne, ns, v, isRet) <- doStms stms e s VUnit
    return(ne, ns, v)

--bedzie ustawiac flagi dla break i continue
doStms :: [Stm] -> Env -> State -> Value -> IO(Env, State, Value, Bool)
doStms stms e s v = case stms of
    [] -> return(e, s, v, True)
    h:t -> do
--        putStrLn $ show h
        (ne, ns, nv, isRet) <- transStm h e s
        case isRet of
          True -> return(ne, ns, nv, isRet)
          False -> doStms t ne ns nv





-- ----------------------- --
-- D E C L A R A T I O N S --
-- ----------------------- --

declare :: Env -> State -> Ident -> Value -> Bool -> IO(Env, State)
declare (E em) (S sm) id val const = do
    let VInt index = sm ! 0
    let nem = insert id (index, const) em
    let nsm = insert index val sm
    let nnsm = insert 0 (VInt (index+1)) nsm
--    putStrLn ""
--    putStrLn ""
--    putStrLn "DECLARE CALLED"
--    putStrLn ""
--    putStrLn (Data.Map.Internal.Debug.showTree nem)
--    putStrLn (Data.Map.Internal.Debug.showTree nnsm)
--    putStrLn ""
    return(E nem, S nnsm)

declareFun :: Env -> State -> Ident -> [Arg] -> [Stm] -> Bool -> IO(Env, State)
declareFun (E em) (S sm) id args stms const = do
    let VInt index = sm ! 0
    let ne = E (insert id (index, const) em)
    let nsm = insert index (VFun args stms ne) sm
    let nnsm = insert 0 (VInt (index+1)) nsm
    return(ne, S nnsm)

transDec :: Dec -> Env -> State -> IO(Env, State)
transDec x e s = case x of
  Dfun functiondec -> transFunctionDec functiondec e s
  Darray arraydec -> failure x
  Dvar ident type_ exp -> do
      (ns, val) <- transExp exp e s
      declare e ns ident val False
  Dval ident type_ exp -> do
      (ns, val) <- transExp exp e s
      declare e ns ident val True
  Dvarnull ident type_ -> do
      (ns, val) <- transExp Enull e s
      declare e ns ident val False
  Dvalnull ident type_ -> do
      (ns, val) <- transExp Enull e s
      declare e ns ident val True

transFunctionDec :: FunctionDec -> Env -> State -> IO(Env, State)
transFunctionDec x e s = case x of
  FunDec (Ident "main") _ _ stms -> do
      (ne, ns, v) <- doFun stms e s
      return (e, ns)
--  FunDec (Ident "main") _ _ stms -> do
--      error "Function 'main' shloud look like:\n  fun main(): Unit { }"
--      return(e,s)
  FunDec ident args type_ stms -> declareFun e s ident args stms True




-- --------------- --
-- ---- I D K ---- --
-- --------------- --

transDimExp :: DimExp -> IO()
transDimExp x = case x of
  Dim exp -> failure x
transOpAssign :: OpAssign -> IO()
transOpAssign x = case x of
  OpAssign1 -> failure x
  OpAssign2 -> failure x
  OpAssign3 -> failure x
  OpAssign4 -> failure x
  OpAssign5 -> failure x
transBaseType :: BaseType -> IO()
transBaseType x = case x of
  Ttupla types -> failure x
  Tbool -> failure x
  Tint -> failure x
  Tstring -> failure x
transType :: Type -> IO()
transType x = case x of
  Tunit -> failure x
  Tnull basetype -> failure x
  Tnonnull basetype -> failure x
  Tfun types basetype -> failure x
transArg :: Arg -> IO()
transArg x = case x of
  Args ident type_ -> failure x
transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transLambda :: Lambda -> IO()
transLambda x = case x of
  LambdaRet args stms exp -> failure x
  LambdaNoRet args stms -> failure x
transIterable :: Iterable -> IO()
transIterable x = case x of
  Itarray ident -> failure x
  Itrange exp1 exp2 -> failure x
  Itup exp1 exp2 -> failure x
  Itdown exp1 exp2 -> failure x
  Itupst exp1 exp2 exp3 -> failure x
  Itdownst exp1 exp2 exp3 -> failure x
transArrayDec :: ArrayDec -> IO()
transArrayDec x = case x of
  ArrDec ident exp1 exp2 -> failure x



-- ------------------- --
-- S T A T E M E N T S --
-- ------------------- --

transStm :: Stm -> Env -> State -> IO(Env, State, Value, Bool)
transStm x e s  = case x of
  Sdec dec -> do
      (ne, se) <- transDec dec e s
      return(ne, se, VUnit, False)
  Sexp exp -> do
      (ns, v) <- transExp exp e s
      return(e, ns, v, False)
  Sblock stms -> do
      (_, ns, _, _) <- doStms stms e s VUnit
      return(e, ns, VUnit, False)
  Sfor ident iterable stms -> failure x
  Swhile exp stms -> failure x
  Sbreak -> failure x
  Scont -> failure x
  Sretexp exp -> do
      (ns, v) <- transExp exp e s
      return(e, ns, v, True)
  Sret -> return(e, s, VUnit, True)
  Sif exp stms -> do
      (ns, VBool v) <- transExp exp e s
      case v of
        True -> do
            (_, nns, _, _) <- doStms stms e ns VUnit
            return(e, nns, VUnit, False)
        False -> return(e, s ,VUnit, False)
  Sifelse exp stms1 stms2 -> do
      (ns, VBool v) <- transExp exp e s
      case v of
        True -> do
            (_, nns, _, _) <- doStms stms1 e ns VUnit
            return(e, nns, VUnit, False)
        False -> do
            (_, nns, _, _) <- doStms stms2 e ns VUnit
            return(e, nns, VUnit, False)
  Sprint exp -> do
      (ns, x) <- transExp exp e s
      case x of
        VInt v -> putStr (show v)
        VString v -> putStr (show v)
        VBool v -> putStr (show v)
      return(e, ns, VUnit, False)
  Sprintln exp -> do
      (ne, ns, v, isRet) <- transStm (Sprint exp) e s
      putStrLn ""
      return(ne, ns, v, isRet)
  Snotnull exp stm -> failure x





-- ------------------- --
-- E X P R E S I O N S --
-- ------------------- --

addArgsHelper :: [Arg] -> [Exp] -> Env -> Env -> State -> IO(Env, State)
addArgsHelper args exps eToAdd eToEval s = case (args, exps) of
    ([], []) -> return(eToAdd, s)
    (Args ident _:at, exp:et) -> do
        (ns, v) <- transExp exp eToEval s
        (ne, nns) <- declare eToAdd s ident v False
        addArgsHelper at et ne eToEval nns

transFunctionExp :: FunctionExp -> Env -> State -> IO(State, Value)
transFunctionExp x e@(E em) s@(S sm) = case x of
  FunCall ident exps -> do
      let (index, const) = em ! ident
      let VFun args stms ne = sm ! index
      (nne, ns) <- addArgsHelper args exps ne e s
      (_, nns, v) <- doFun stms nne ns
      return(nns, v)

transEtuplaHelper :: [Exp] -> Env -> State -> IO(State, [Value])
transEtuplaHelper exps e s = case exps of
  [] -> return(s, [])
  h:t -> do
      (ns, nv) <- transExp h e s
      (nns, nvs) <- transEtuplaHelper t e ns
      return(nns, nv:nvs)

transIntHelper :: Exp -> Exp -> (Integer -> Integer -> Integer) -> Env -> State -> IO(State, Value)
transIntHelper exp1 exp2 f e s = do
    (ns, VInt a) <- transExp exp1 e s
    (nns, VInt b) <- transExp exp2 e ns
    return(nns, VInt (f a b))

transBoolHelper :: Exp -> Exp -> (Value -> Value -> Bool) -> Env -> State -> IO(State, Value)
transBoolHelper exp1 exp2 f e s = do
    (ns, a) <- transExp exp1 e s
    (nns, b) <- transExp exp2 e ns
    return(nns, VBool (f a b))


transExp :: Exp -> Env -> State -> IO(State, Value)
transExp x e@(E em) s@(S sm) = case x of
  Eassign (Ear ident) opassign exp -> do
      (ns, val) <- transExp exp e s
      let (index, const) = em ! ident
      let nsm = insert index val sm
      return(S nsm, val)
  Eternary exp1 exp2 exp3 -> failure x
  Eor exp1 exp2 -> do
      (ns, VBool a) <- transExp exp1 e s
      (nns, VBool b) <- transExp exp2 e ns
      return(nns, VBool (a || b))
  Eand exp1 exp2 -> do
      (ns, VBool a) <- transExp exp1 e s
      (nns, VBool b) <- transExp exp2 e ns
      return(nns, VBool (a && b))
  Eeq exp1 exp2 -> transBoolHelper exp1 exp2 (==) e s
  Eneq exp1 exp2 -> transBoolHelper exp1 exp2 (/=) e s
  El exp1 exp2 -> transBoolHelper exp1 exp2 (<) e s
  Eg exp1 exp2 -> transBoolHelper exp1 exp2 (>) e s
  Ele exp1 exp2 -> transBoolHelper exp1 exp2 (<=) e s
  Ege exp1 exp2 -> transBoolHelper exp1 exp2 (>=) e s
  Eadd exp1 exp2 -> transIntHelper exp1 exp2 (+) e s
  Esub exp1 exp2 -> transIntHelper exp1 exp2 (-) e s
  Emul exp1 exp2 -> transIntHelper exp1 exp2 (*) e s
  Ediv exp1 exp2 -> do
      (ns, a) <- transExp exp1 e s
      (nns, b) <- transExp exp2 e ns
      case (a, b) of
        (_, VInt 0) -> error "Cannot divide by 0"
        (VInt va, VInt vb) -> return(nns, VInt (div va vb))
  Emod exp1 exp2 -> transIntHelper exp1 exp2 mod e s
  Eneg exp -> do
      (ns, VBool a) <- transExp exp e s
      return(ns, VBool (not a))
  Elneg exp -> failure x
  Einc exp -> failure x
  Edec exp -> failure x
  Etupla exps -> do
      (ns, vs) <- transEtuplaHelper exps e s
      return(ns, VTupla (reverse vs))
  Eint integer -> return(s, VInt integer)
  Estring string -> return(s, VString string)
  Etrue -> return(s, VBool True)
  Efalse -> return(s, VBool False)
  Enull -> return(s, VNull)
  Ecall functionexp -> transFunctionExp functionexp e s
  Eget ident dimexps -> failure x
  Elambda lambda -> failure x
  Ennass exp -> failure x
  Ear ident -> do
      let (index, c) = em ! ident
      let v =  sm ! index
      return(s, v)