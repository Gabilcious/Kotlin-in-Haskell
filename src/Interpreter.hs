module Interpreter where

import AbsKotlin
import ErrM
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import Control.Exception
import GHC.IO.Exception

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
  deriving (Eq, Ord, Read)

instance Show Value where
      show (VNull) = "null"
      show (VInt i) = show i
      show (VString s) = s
      show (VBool b) = show b
      show (VTupla []) = "()";
      show (VTupla list) = "(" ++ (foldr1 (\x y -> x ++ "," ++ y) (Prelude.map show list)) ++ ")";
      show (VArray []) = "[]";
      show (VArray list) = "[" ++ (foldr1 (\x y -> x ++ "," ++ y) (Prelude.map show list)) ++ "]";

data Env = E (Map Ident (Integer, Bool))
  deriving (Eq, Ord, Show, Read)

data State = S (Map Integer Value)
  deriving (Eq, Ord, Show, Read)




-- ------------- --
-- P R O G R A M --
-- ------------- --

start :: Prog -> IO()
start prog = catch (
    do
      let e = E empty
      let s = S (fromList [(-1, VInt 1), (-2, go)])
      transProg prog e s) $ \ex -> do
          throw $ AssertionFailed ("Error has occured:\n\t" ++ show (ex :: SomeException) )

transProg :: Prog -> Env -> State -> IO()
transProg x e@(E em) s@(S sm) = case x of
  Program [] -> return ()
  Program (h:t) -> do
      (ne, ns) <- transInst h e s
      transProg (Program t) ne ns

transInst :: Inst -> Env -> State -> IO(Env, State)
transInst x = case x of
  Idec dec -> transDec dec





-- ----------------- --
-- ----- M A P ----- --
-- ----------------- --

assign :: Env -> State -> Ident -> Value -> State
assign (E em) (S sm) id val = let
    (index, _) = (em ! id)
  in S (insert index val sm)


assignArrayHelper :: Env -> State -> [Value] -> [DimExp] -> Value -> IO(Value)
assignArrayHelper e s vals [Dim dim] change = do
    (ns, VInt idx) <- transExp dim e s
    nv <- mapM (\(i, v) ->
       if i == idx then return(change)
       else return(v)) (zip [0..] vals)
    return(VArray nv)

assignArrayHelper e s vals (Dim dim:dims) change = do
    (ns, VInt idx) <- transExp dim e s
    nv <- mapM (\(i, VArray v) ->
        if i == idx then assignArrayHelper e ns v dims change
        else return(VArray v)) (zip [0..] vals)
    return(VArray nv)

assignArray :: Env -> State -> Ident -> [DimExp] -> Value -> IO(State)
assignArray e@(E em) s@(S sm) id dims val = do
    let (index, _) = (em ! id)
    let VArray v = (sm ! index)
    nv <- assignArrayHelper e s v dims val
    return(S (insert index nv sm))

alloc :: Env -> State -> Ident -> Bool -> (Env, State)
alloc (E em) (S sm) ident const = let
    VInt index = sm ! (-1)
    ns = S (insert (-1) (VInt (index+1)) sm)
    ne = E (insert ident (index, const) em)
  in (ne, ns)

getIdx :: State -> Integer -> Value
getIdx (S sm) index = sm ! index

getVal :: Env -> State -> Ident -> Value
getVal (E em) s ident = let
    (index, _) = em ! ident
  in getIdx s index


ret = VString "RETURN"
bre = VString "BREAK"
cont = VString "CONT"
go = VString "GO"


set :: Value -> State -> State
set v (S sm) = S (insert (-2) v sm)

get :: State -> Value
get (S sm) = sm ! (-2)

is :: Value -> State -> Bool
is v s = v == get s





-- ----------------------- --
-- D E C L A R A T I O N S --
-- ----------------------- --

declare :: Env -> State -> Ident -> Value -> Bool -> IO(Env, State)
declare e s id val const = do
    let (ne, ns) = alloc e s id const
    let nns = assign ne ns id val
    return(ne, nns)

declareFun :: Env -> State -> Ident -> [Arg] -> [Stm] -> Bool -> IO(Env, State)
declareFun e s id@(Ident name) args stms const = catch (do
    let (ne, ns) = alloc e s id const
    let nns = assign ne ns id (VFun args stms ne)
    return(ne, nns)) $ \ex ->
               throwIO $ AssertionFailed ("in declaration " ++ name ++ ":\n\t" ++ show (ex :: SomeException) )

transDec :: Dec -> Env -> State -> IO(Env, State)
transDec x e s = case x of
  Dfun functiondec -> transFunctionDec functiondec e s
  Dvar ident@(Ident name) type_ exp -> catch (do
      (ns, val) <- transExp exp e s
      declare e ns ident val False) $ \ex ->
              throwIO $ AssertionFailed ("in declaration " ++ name ++ ":\n\t" ++ show (ex :: SomeException) )
  Dval ident@(Ident name) type_ exp -> catch (do
      (ns, val) <- transExp exp e s
      declare e ns ident val True) $ \ex ->
              throwIO $ AssertionFailed ("in declaration " ++ name ++ ":\n\t" ++ show (ex :: SomeException) )
  Dvarnull ident@(Ident name) type_ -> catch (do
      (ns, val) <- transExp Enull e s
      declare e ns ident val False) $ \ex ->
              throwIO $ AssertionFailed ("in declaration " ++ name ++ ":\n\t" ++ show (ex :: SomeException) )
  Dvalnull ident@(Ident name) type_ -> catch (do
      (ns, val) <- transExp Enull e s
      declare e ns ident val True) $ \ex ->
              throwIO $ AssertionFailed ("in declaration " ++ name ++ ":\n\t" ++ show (ex :: SomeException) )

transFunctionDec :: FunctionDec -> Env -> State -> IO(Env, State)
transFunctionDec x e s = case x of
  FunDec (Ident "main") _ _ stms -> do
      (_, ns, v) <- catch (doStms stms e s) $ \ex ->
              throw $ AssertionFailed ("in function main:\n\t" ++ show (ex :: SomeException) )
      return (e, set go ns)
  FunDec ident args type_ stms -> declareFun e s ident args stms True

transArray :: Env -> State -> [Stm] -> Ident -> Integer -> Integer -> [Value] -> IO(State, [Value])
transArray e s stms it size expected vals = case size < expected of
    True -> do
      (ne, ns) <- declare e s it (VInt size) False
      (nne, nns, v) <- doStms stms ne ns
      transArray nne (set go nns) stms it (size+1) expected (v:vals)
    False -> return(s, reverse vals)





-- --------------- --
-- I T E R A B L E --
-- --------------- --

transIterableHelper :: Env -> State -> Exp -> Exp -> Exp -> Bool -> IO(State, [Value])
transIterableHelper e s av bv stepv inclusive = do
    (ns, VInt a) <- transExp av e s
    (nns, VInt c) <- transExp bv e ns
    (nnns, VInt step) <- transExp stepv e ns
    let b = a + step
    case (inclusive, step > 0) of
      (True, _) -> return(nnns, Prelude.map (\x -> VInt x) [a,b..c])
      (False, True) -> return(nnns, Prelude.map (\x -> VInt x) [a,b..(c-1)])
      (False, False) -> return(nnns, Prelude.map (\x -> VInt x) [a,b..(c+1)])

transIterable :: Iterable -> Env -> State -> IO(State, [Value])
transIterable x e s = case x of
  Itrange exp1 exp2 -> transIterableHelper e s exp1 exp2 (Eint 1) True
  Itup exp1 exp2 -> transIterableHelper e s exp1 exp2 (Eint 1) False
  Itdown exp1 exp2 -> transIterableHelper e s exp1 exp2 (Eint (-1)) False
  Itupst exp1 exp2 exp3 -> transIterableHelper e s exp1 exp2 exp3 False
  Itdownst exp1 exp2 exp3 -> transIterableHelper e s exp1 exp2 (Eneg exp3) False





-- ------------------- --
-- S T A T E M E N T S --
-- ------------------- --

doStmsHelper :: [Stm] -> Env -> State -> Value -> IO(Env, State, Value)
doStmsHelper stms e s v = case stms of
    [] -> return(e, s, v)
    h:t -> do
        (ne, ns, nv) <- transStm h e s
        case is go ns of
          False -> return(ne, ns, nv)
          True -> doStmsHelper t ne ns nv

doStms :: [Stm] -> Env -> State -> IO(Env, State, Value)
doStms stms e s = doStmsHelper stms e s VUnit


doFor :: Ident -> [Value] -> [Stm] -> Value -> Env -> State -> IO(State, Value)
doFor ident list stms v e s = case list of
    [] -> return(s, v)
    h:t -> do
      let ns = assign e s ident h
      (_, nns, nv) <- doStms stms e ns
      case get nns of
              x | x == ret  -> return(ns, v)
                | x == bre  -> return(set go ns, v)
                | x == cont -> doFor ident t stms nv e (set go nns)
                | x == go   -> doFor ident t stms nv e nns

doWhile :: Exp -> [Stm] -> Env -> State -> IO(Env, State, Value)
doWhile exp stms e s = do
    (ns, VBool vb) <- transExp exp e s
    (_, nns, v) <- case vb of
       True -> do
          (_, nns, v) <- doStms stms e ns
          return(e, nns, v)
       False -> do
          return(e, ns, VUnit)
    case get nns of
        x | x == ret  -> return(e, nns, v)
          | x == bre  -> return(e, set go nns, v)
          | x == cont -> doWhile exp stms e (set go nns)
          | x == go   -> doWhile exp stms e nns

transStm :: Stm -> Env -> State -> IO(Env, State, Value)
transStm x e s  = case x of
  Sdec dec -> do
      (ne, se) <- transDec dec e s
      return(ne, se, VUnit)
  Sexp exp -> catch (do
      (ns, v) <- transExp exp e s
      return(e, ns, v)) $ \ex -> do
           throwIO $ AssertionFailed ("in statement (" ++ show exp ++"):\n\t" ++ show (ex :: SomeException) )
  Sblock stms -> catch (do
      (_, ns, v) <- doStms stms e s
      return(e, ns, v) ) $ \ex ->
           throwIO $ AssertionFailed ("in block statemenet:\n\t" ++ show (ex :: SomeException) )
  Sfor ident exp stms -> catch (do
      let (ne, ns) = alloc e s ident True
      (nns, VArray list) <- transExp exp ne ns
      (nnns, v) <- doFor ident list stms VUnit ne nns
      return(e, nnns, v) ) $ \ex ->
             throwIO $ AssertionFailed ("in for statemenet:\n\t" ++ show (ex :: SomeException) )
  Swhile exp stms -> catch (doWhile exp stms e s) $ \ex ->
             throwIO $ AssertionFailed ("in while statemenet:\n\t" ++ show (ex :: SomeException) )
  Sbreak -> return(e, set bre s, VUnit)
  Scont -> return(e, set cont s, VUnit)
  Sretexp exp -> catch (do
      (ns, v) <- transExp exp e s
      return(e, set ret ns, v) ) $ \ex ->
         throwIO $ AssertionFailed ("in return statemenet:\n\t" ++ show (ex :: SomeException) )
  Sret -> return(e, set ret s, VUnit)
  Sif exp stms -> catch (do
      (ns, VBool v) <- transExp exp e s
      case v of
        True -> do
            (_, nns, v) <- doStms stms e ns
            return(e, nns, v)
        False -> return(e, ns ,VUnit) ) $ \ex ->
            throwIO $ AssertionFailed ("in if statemenet:\n\t" ++ show (ex :: SomeException) )
  Sifelse exp stms1 stms2 -> catch (do
      (ns, VBool v) <- transExp exp e s
      case v of
        True -> do
            (_, nns, v) <- doStms stms1 e ns
            return(e, nns, v)
        False -> do
            (_, nns, v) <- doStms stms2 e ns
            return(e, nns, v) ) $ \ex ->
        throwIO $ AssertionFailed ("in if-else statemenet:\n\t" ++ show (ex :: SomeException) )
  Sprint exp -> catch (do
      (ns, x) <- transExp exp e s
      putStr (show x)
      return(e, ns, VUnit) ) $ \ex ->
         throwIO $ AssertionFailed ("in print statemenet:\n\t" ++ show (ex :: SomeException) )
  Sprintln exp -> catch (do
      (ns, x) <- transExp exp e s
      putStrLn (show x)
      return(e, ns, VUnit) ) $ \ex ->
         throwIO $ AssertionFailed ("in println statemenet:\n\t" ++ show (ex :: SomeException) )
  Snotnull exp stms -> catch (do
      (ns, v) <- transExp exp e s
      case v of
        VNull -> return(e, ns, VUnit)
        _ -> transStm (Sblock stms) e s ) $ \ex ->
          throwIO $ AssertionFailed ("in ?? statemenet:\n\t" ++ show (ex :: SomeException) )
  Sassert exp -> do
      (ns, VBool b) <- catch (transExp exp e s) $ \ex ->
          throwIO $ AssertionFailed ("in assert statemenet:\n\t" ++ show (ex :: SomeException) )
      if b then return(e, ns, VUnit)
      else throwIO $ AssertionFailed ("Assertion failed " ++ show exp)





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
transFunctionExp x e s = case x of
  FunCall ident@(Ident name) exps -> catch (do
      let VFun args stms ne = getVal e s ident
      (nne, ns) <- addArgsHelper args exps ne e s
      (_, nns, v) <- doStms stms nne ns
      return(set go nns, v))
       $ \ex ->
           throw $ AssertionFailed ("in function " ++ name ++ " call:\n\t" ++ show (ex :: SomeException) )


transDimExp :: DimExp -> Env -> State -> IO(State, Value)
transDimExp x e s = case x of
  Dim exp -> transExp exp e s

transGetExpHelper :: Value -> [DimExp] -> Env -> State -> IO(State, Value)
transGetExpHelper v dims e s = case (v,dims) of
    (_, []) -> return(s,v)
    (VArray list, h:t) -> do
      (ns, VInt idx) <- transDimExp h e s
      case fromIntegral idx  of
        index | index < 0 -> throwIO $ AssertionFailed ("index " ++ show index ++ " is negative")
              | index >= length list -> throwIO $ AssertionFailed ("index " ++ show index ++ " is out of bound")
              | otherwise -> transGetExpHelper (list !! index) t e ns

transGetExp :: Ident -> [DimExp] -> Env -> State -> IO(State, Value)
transGetExp ident@(Ident name) dimexps e s = catch (transGetExpHelper (getVal e s ident) dimexps e s) $ \ex ->
    throw $ AssertionFailed ("in array " ++ name ++ ":\n\t" ++ show (ex :: SomeException) )




transEtuplaHelper :: [Exp] -> Env -> State -> IO(State, [Value])
transEtuplaHelper exps e s = case exps of
  [] -> return(s, [])
  h:t -> do
      (ns, nv) <- transExp h e s
      (nns, nvs) <- transEtuplaHelper t e ns
      return(nns, nv:nvs)

transHelper :: Exp -> Exp -> (Value -> Value -> Value) -> Env -> State -> IO(State, Value)
transHelper exp1 exp2 f e s = do
    (ns, a) <- transExp exp1 e s
    (nns, b) <- transExp exp2 e ns
    return(nns, f a b)

transBoolHelper :: Exp -> Exp -> (Value -> Value -> Bool) -> Env -> State -> IO(State, Value)
transBoolHelper exp1 exp2 f e s = do
    (ns, a) <- transExp exp1 e s
    (nns, b) <- transExp exp2 e ns
    return(nns, VBool (f a b))


transExp :: Exp -> Env -> State -> IO(State, Value)
transExp x e s = case x of
  Eassign exp1@(Evar ident) opassign exp2 -> do
      (ns, val) <- transHelper exp1 exp2 (transOpAssign opassign) e s
      let nns = assign e ns ident val
      return(nns, val)
  Eassign exp1@(Eget ident dimexps) opassign exp2 -> do
      (ns, val) <- transHelper exp1 exp2 (transOpAssign opassign) e s
      nns <- assignArray e ns ident dimexps val
      return(nns, val)
  Eternary exp1 exp2 exp3 -> do
      (ns, VBool b) <- transExp exp1 e s
      case b of
        True -> transExp exp2 e s
        False -> transExp exp3 e s
  Eor exp1 exp2 -> do
      (ns, VBool a) <- transExp exp1 e s
      if a then return (ns, VBool True)
      else do
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
  Eadd exp1 exp2 -> transHelper exp1 exp2 (transOpAssign OpAssign2) e s
  Esub exp1 exp2 -> transHelper exp1 exp2 (transOpAssign OpAssign3) e s
  Emul exp1 exp2 -> transHelper exp1 exp2 (transOpAssign OpAssign4) e s
  Ediv exp1 exp2 -> transHelper exp1 exp2 (transOpAssign OpAssign5) e s
  Emod exp1 exp2 -> transHelper exp1 exp2 (transOpAssign OpAssign6) e s
  Eneg exp -> do
      (ns, VInt a) <- transExp exp e s
      return(ns, VInt (- a))
  Elneg exp -> do
      (ns, VBool a) <- transExp exp e s
      return(ns, VBool (not a))
  Einc (Evar ident) -> do
      let VInt v = getVal e s ident
      let ns = assign e s ident (VInt (v+1))
      return(ns, VInt (v+1))
  Edec (Evar ident) -> do
      let VInt v = getVal e s ident
      let ns = assign e s ident (VInt (v-1))
      return(ns, VInt (v-1))
  EPinc (Evar ident) -> do
      let VInt v = getVal e s ident
      let ns = assign e s ident (VInt (v+1))
      return(ns, VInt v)
  EPdec (Evar ident) -> do
      let VInt v = getVal e s ident
      let ns = assign e s ident (VInt (v-1))
      return(ns, VInt v)
  Eiter iterable -> do
      (ns, vs) <- transIterable iterable e s
      return(ns, VArray vs)
  Earray size exp -> do
      (ns, VInt v) <- transExp size e s
      if v < 0 then throwIO $ AssertionFailed ("Array size is nagative: " ++ show v)
      else do
        (nns, VFun [Args it _] stms eFun) <- transExp exp e ns
        (nnns, vs) <- transArray e nns stms it 0 v []
        return(nnns, VArray vs)
  Etupla exps -> do
      (ns, vs) <- transEtuplaHelper exps e s
      return(ns, VTupla vs)
  Eint integer -> return(s, VInt integer)
  Estring string -> return(s, VString string)
  Etrue -> return(s, VBool True)
  Efalse -> return(s, VBool False)
  Enull -> return(s, VNull)
  Ecall functionexp -> transFunctionExp functionexp e s
  Eget ident dimexps -> transGetExp ident dimexps e s
  Elambda args stms -> return(s, VFun args stms e)
  Ennass exp -> do
      (ns, v) <- transExp exp e s
      case v of
        VNull -> throwIO $ AssertionFailed ("Null pointer exeption")
        _ -> return(ns,v)
  Evar ident -> return(s, getVal e s ident)

transOpAssign :: OpAssign -> Value -> Value -> Value
transOpAssign x a b = case x of
  OpAssign1 -> b
  OpAssign2 -> case (a, b) of
    (VInt va, VInt vb) -> VInt (va + vb)
    (VString va, VString vb) -> VString (va ++ vb)
  OpAssign3 -> case (a, b) of
    (VInt va, VInt vb) -> VInt (va - vb)
  OpAssign4 -> case (a, b) of
    (VInt va, VInt vb) -> VInt (va * vb)
  OpAssign5 -> case (a, b) of
    (VInt va, VInt 0) -> throw $ AssertionFailed ("Cannot divide by 0")
    (VInt va, VInt vb) -> VInt (div va  vb)
  OpAssign6 -> case (a, b) of
    (VInt va, VInt vb) -> VInt (mod va vb)