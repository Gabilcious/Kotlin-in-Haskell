module TypeCheck where

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

-- ident  ->  index x const x depth
data Env = E (Map Ident (Integer, Bool, Integer))
  deriving (Eq, Ord, Show, Read)

data State = S (Map Integer Type)
  deriving (Eq, Ord, Show, Read)

failure x = error ("Undefined case: " ++ show x)




-- ------------- --
-- P R O G R A M --
-- ------------- --

check :: Prog -> IO()
check prog = do
    let e = E empty
--    -1 index     -2 howManyLoops     -3 depth     -4 main
    let s = S (fromList [(-1, Help 1), (-2, Help 0), (-3, Help 0), (-4, Help 0)])
    S nsm <- transProg prog e s
    case nsm ! (-4) of
      Help 1 -> return()
      otherwise -> error "Main was not found"

transProg :: Prog -> Env -> State -> IO(State)
transProg x e@(E em) s@(S sm) = case x of
  Program [] -> do
      putStrLn ""
      putStrLn ""
      putStrLn ""
      putStrLn (Data.Map.Internal.Debug.showTree em)
      putStrLn (Data.Map.Internal.Debug.showTree sm)
      return(s)
  Program (h:t) -> do
      (ne, ns) <- transInst h e s
      transProg (Program t) ne ns
--
transInst :: Inst -> Env -> State -> IO(Env, State)
transInst x = case x of
  Idec dec -> transDec dec





-- ----------------- --
-- ----- M A P ----- --
-- ----------------- --

canAssignB :: BaseType -> BaseType -> Bool
canAssignB x y = case (x, y) of
    (a, b) | a == b       -> True
    (Ttupla a, Ttupla b)  -> Prelude.foldl (&&) True (zipWith canAssign a b)
    otherwise             -> False

canAssign :: Type -> Type -> Bool
canAssign x y = case (x, y) of
    (a, b) | a == b            -> True
    (Tfun _ _,    _)           -> False
    (_,           Tfun _ _)    -> False
    (Tnonnull _,  Tnull)       -> False
    (Tnullable _, Tnull)       -> True
    (Tnullable a, Tnonnull b)  -> canAssignB a b
    (Tnullable _, Tnullable _) -> False
    (Tnonnull a,  Tnonnull b)  -> canAssignB a b
    (Tnonnull a,  Tnullable b) -> canAssignB a b

-- try to do: a = b
tryAssign :: Type -> Type -> IO(Type)
tryAssign a b = do
--  putStrLn (show a)
--  putStrLn (show b)
  case canAssign a b of
    True -> return(a)
    False -> error ("Cannot assign " ++ (show b) ++ " to " ++ (show a))

-- TODO: gorliwośc ifa usunac
alloc :: Env -> State -> Ident -> Bool -> Type -> IO(Env, State)
alloc (E em) (S sm) ident const t = do
--check if can be declared
    let Help depth = sm ! (-3)
    case ident of
      Ident "main" -> error "Main is keyword"
      otherwise ->
          case Data.Map.lookup ident em of
            Just (_, _, d)  | d == depth -> error (show ident ++ " was previously declared in this scope")
            otherwise -> do
              let Help index = sm ! (-1)
              let ne = E (insert ident (index, const, depth) em)
              let nsm = insert (-1) (Help (index+1)) sm
              let nns = S (insert index  t nsm)
              return(ne, nns)

getIdx :: State -> Integer -> IO(Type)
getIdx (S sm) index = return(sm ! index)

getVal :: Env -> State -> Ident -> IO(Type)
getVal (E em) s ident =  case Data.Map.lookup ident em of
    Nothing -> error ( show ident ++ " is not defined")
    Just (index, _, _) -> getIdx s index

assertNotConst :: Env -> Ident -> IO()
assertNotConst e@(E em) ident = do
    let (_, const, _) = em ! ident
    case const of
      True -> error ("Val cannot be reasigned")
      False -> return()

incDepth :: State -> State
incDepth (S sm) = let
    Help depth = sm ! (-3)
    in S (insert (-3) (Help (depth+1)) sm)



-- ----------------------- --
-- D E C L A R A T I O N S --
-- ----------------------- --

declare :: Env -> State -> Ident -> Type -> Bool -> Exp -> IO(Env, State)
declare e s ident a const exp = do
      (ne, ns) <- alloc e s ident const a
      (nns, b) <- transExp exp ne ns
      _ <- tryAssign a b
      return(ne, nns)

--declareFun :: Env -> State -> Ident -> [Arg] -> [Stm] -> Bool -> IO(Env, State)
--declareFun e s id args stms const = do
--    let (ne, ns) = alloc e s id const
--    let nns = assign ne ns id (VFun args stms ne)
--    return(ne, nns)

transDec :: Dec -> Env -> State -> IO(Env, State)
transDec x e s = case x of
  Dfun functiondec -> transFunctionDec functiondec e s
--  Darray arraydec -> transArrayDec arraydec e s
  Dvar ident t exp -> declare e s ident t False exp
  Dval ident type_ exp -> declare e s ident type_ True exp
  Dvarnull ident type_ -> declare e s ident type_ False Enull
  Dvalnull ident type_ -> declare e s ident type_ True Enull

transFunctionDec :: FunctionDec -> Env -> State -> IO(Env, State)
transFunctionDec x e s@(S sm) = case x of
  FunDec (Ident "main") [] TRunit stms -> do
      case sm ! (-4) of
        Help 0 -> do
          let ns = S (insert (-4) (Help 1) sm)
          let nns = incDepth ns
          (_, _, v) <- doStms stms e nns
          return (e, ns)
        otherwise -> error "Main was previously declared"
  FunDec (Ident "main") _ _ _ -> error "Type of main shoud be: () -> Unit"
--  FunDec ident args type_ stms -> declareFun e s ident type_

--transArrayDecHelper :: Env -> State -> [Stm] -> Ident -> Integer -> Integer -> [Value] -> IO(State, [Value])
--transArrayDecHelper e s stms it size expected vals = case size < expected of
--    True -> do
--      (ne, ns) <- declare e s it (VInt size) False
--      (nne, nns, v) <- doStms stms ne ns
--      transArrayDecHelper nne (set go nns) stms it (size+1) expected (v:vals)
--    False -> return(s, reverse vals)
--
--transArrayDec :: ArrayDec -> Env -> State -> IO(Env, State)
--transArrayDec x e s = case x of
--  ArrDec ident (Eint size) exp -> do
--      (ns, VFun [Args it _] stms eFun) <- transExp exp e s
--      (nns, list) <- transArrayDecHelper e ns stms it 0 size []
--      declare e nns ident (VArray list) True
--  ArrItDec ident iterable -> do
--      (ns, list) <- transIterable iterable e s
--      declare e ns ident (VArray list) True




-- --------------- --
-- I T E R A B L E --
-- --------------- --

--transIterableHelper :: Env -> State -> Exp -> Exp -> Exp -> Bool -> IO(State, [Value])
--transIterableHelper e s av bv stepv inclusive = do
--    (ns, VInt a) <- transExp av e s
--    (nns, VInt c) <- transExp bv e ns
--    (nnns, VInt step) <- transExp stepv e ns
--    let b = a + step
--    case (inclusive, step > 0) of
--      (True, _) -> return(nnns, Prelude.map (\x -> VInt x) [a,b..c])
--      (False, True) -> return(nnns, Prelude.map (\x -> VInt x) [a,b..(c-1)])
--      (False, False) -> return(nnns, Prelude.map (\x -> VInt x) [a,b..(c+1)])
--
--transIterable :: Iterable -> Env -> State -> IO(State, [Value])
--transIterable x e s = case x of
--  Itarray ident -> do
--      let VArray list = getVal e s ident
--      return(s, list)
--  Itrange exp1 exp2 -> transIterableHelper e s exp1 exp2 (Eint 1) True
--  Itup exp1 exp2 -> transIterableHelper e s exp1 exp2 (Eint 1) False
--  Itdown exp1 exp2 -> transIterableHelper e s exp1 exp2 (Eint (-1)) False
--  Itupst exp1 exp2 exp3 -> transIterableHelper e s exp1 exp2 exp3 False
--  Itdownst exp1 exp2 exp3 -> transIterableHelper e s exp1 exp2 (Eneg exp3) False





-- ------------------- --
-- S T A T E M E N T S --
-- ------------------- --

doStmsHelper :: [Stm] -> Env -> State -> RetType -> IO(Env, State, RetType)
doStmsHelper stms e s typ = case stms of
    [] -> return(e, s, typ)
    h:t -> do
        (ne, ns, ntyp) <- transStm h e s
        doStmsHelper t ne ns ntyp

doStms :: [Stm] -> Env -> State -> IO(Env, State, RetType)
doStms stms e s = doStmsHelper stms e s TRunit


--doFor :: Ident -> [Value] -> [Stm] -> Value -> Env -> State -> IO(State, Value)
--doFor ident list stms v e s = case list of
--    [] -> return(s, v)
--    h:t -> do
--      let ns = assign e s ident h
--      (_, nns, nv) <- doStms stms e ns
--      case get nns of
--              x | x == ret  -> return(ns, v)
--                | x == bre  -> return(set go ns, v)
--                | x == cont -> doFor ident t stms nv e (set go nns)
--                | x == go   -> doFor ident t stms nv e nns


transStm :: Stm -> Env -> State -> IO(Env, State, RetType)
transStm x e s@(S sm)  = case x of
  Sdec dec -> do
      (ne, se) <- transDec dec e s
      return(ne, se, TRunit)
  Sexp exp -> do
      (_, t) <- transExp exp e s
      return(e, s, TRtype t)
  Sblock stms -> do
      let ns = incDepth s
      (_, nns, t) <- doStms stms e ns
      return(e, s, t)
--  Sfor ident iterable stms -> do
--      let (ne, ns) = alloc e s ident True
--      (nns, list) <- transIterable iterable ne ns
--      (nnns, v) <- doFor ident list stms VUnit ne nns
--      return(e, nnns, v)
  Swhile exp stms -> do
      let Help index = sm ! (-2)
      let nsm = insert (-2) (Help (index+1)) sm
      (_, S nnsm, rt) <- transStm (Sif exp stms) e (S nsm)
      let nnnsm = insert (-2) (Help (index)) nnsm
      return(e, s, rt)
  Sbreak -> do
      loops <- getIdx s (-2)
      case loops of
        Help x | x == 0 -> error "Cannot perform break without a loop"
               | otherwise -> return(e, s, TRunit)
  Scont -> do
      loops <- getIdx s (-2)
      case loops of
        Help x | x == 0 -> error "Cannot perform continue without a loop"
               | otherwise -> return(e, s, TRunit)
  Sretexp exp -> do
      (ns, t) <- transExp exp e s
      return(e, ns, TRtype t)
  Sret -> return(e, s, TRunit)
  Sif exp stms -> do
      (ns, t) <- transExp exp e s
      case t of
        Tnonnull Tbool -> do
          let nns = incDepth ns
          (_,_,t) <- doStms stms e nns
          return (e, s, t)
        otherwise -> error ("Wrong expresion inside if statement: " ++ show t)
  Sifelse exp stms1 stms2 -> do
      (ns, t) <- transExp exp e s
      case t of
        Tnonnull Tbool -> do
          let nns = incDepth ns
          doStms stms1 e nns
          (_,_,t) <- doStms stms2 e ns
          return (e, s, t)
        otherwise -> error ("Wrong expresion inside if statement: " ++ show t)
  Sprint exp -> do
      (ns, t) <- transExp exp e s
      case t of
        Tnullable Tint -> return(e, s, TRunit)
        Tnonnull Tint -> return(e, s, TRunit)
        Tnullable Tstring -> return(e, s, TRunit)
        Tnonnull Tstring -> return(e, s, TRunit)
        Tnullable Tbool -> return(e, s, TRunit)
        Tnonnull Tbool -> return(e, s, TRunit)
        otherwise -> error ("Cannot print " ++ show t)
  Sprintln exp -> transStm (Sprint exp) e s
--  Snotnull exp stms -> do
--      (ns, v) <- transExp exp e s
--      case v of
--        VNull -> return(e, ns, VUnit)
--        _ -> transStm (Sblock stms) e s





-- ------------------- --
-- E X P R E S I O N S --
-- ------------------- --

--addArgsHelper :: [Arg] -> [Exp] -> Env -> Env -> State -> IO(Env, State)
--addArgsHelper args exps eToAdd eToEval s = case (args, exps) of
--    ([], []) -> return(eToAdd, s)
--    (Args ident _:at, exp:et) -> do
--        (ns, v) <- transExp exp eToEval s
--        (ne, nns) <- declare eToAdd s ident v False
--        addArgsHelper at et ne eToEval nns
--
--transFunctionExp :: FunctionExp -> Env -> State -> IO(State, Value)
--transFunctionExp x e s = case x of
--  FunCall ident exps -> do
--      let VFun args stms ne = getVal e s ident
--      (nne, ns) <- addArgsHelper args exps ne e s
--      (_, nns, v) <- doStms stms nne ns
--      return(set go nns, v)
--
--
--transDimExp :: DimExp -> Env -> State -> IO(State, Value)
--transDimExp x e s = case x of
--  Dim exp -> transExp exp e s

--transGetExpHelper :: Value -> [DimExp] -> Env -> State -> IO(State, Value)
--transGetExpHelper v dims e s = case (v,dims) of
--    (_, []) -> return(s,v)
--    (VArray list, h:t) -> do
--      (ns, VInt idx) <- transDimExp h e s
--      case fromIntegral idx  of
--        index | index < 0 -> error "Negative index"
--              | index >= length list -> error "Array Index Out of Bound"
--              | otherwise -> transGetExpHelper (list !! index) t e ns
--
--transGetExp :: Ident -> [DimExp] -> Env -> State -> IO(State, Value)
--transGetExp ident dimexps e s = transGetExpHelper (getVal e s ident) dimexps e s
--
--
--
--transEtuplaHelper :: [Exp] -> Env -> State -> IO(State, [Value])
--transEtuplaHelper exps e s = case exps of
--  [] -> return(s, [])
--  h:t -> do
--      (ns, nv) <- transExp h e s
--      (nns, nvs) <- transEtuplaHelper t e ns
--      return(nns, nv:nvs)

transHelper :: Exp -> Exp -> OpAssign -> Env -> State -> IO(State, Type)
transHelper exp1 exp2 op e s = do
    (ns, a) <- transExp exp1 e s
    (nns, b) <- transExp exp2 e ns
    t <- transOpAssign op a b
    return(nns, t)

transBoolHelper :: Exp -> Exp -> [Char] -> Env -> State -> Integer -> IO(State, Type)
transBoolHelper exp1 exp2 op e s boolInt = do
    (ns, a) <- transExp exp1 e s
    (nns, b) <- transExp exp2 e ns
    case (a, b, boolInt) of
      (Tnonnull Tint, Tnonnull Tint, 2)  -> return(nns, Tnonnull Tbool)
      (Tnonnull Tbool, Tnonnull Tbool, 1) -> return(nns, Tnonnull Tbool)
      otherwise -> error ("Cannot perform " ++ op ++ " on " ++ show a ++ " and " ++ show b)


transExp :: Exp -> Env -> State -> IO(State, Type)
transExp x e s = case x of
  Eassign exp1@(Evar ident) opassign exp2 -> do
      assertNotConst e ident
      transHelper exp1 exp2 opassign e s
  Eternary exp1 exp2 exp3 -> do
      (ns, t) <- transExp exp1 e s
      (_, a) <- transExp exp2 e ns
      (_, b) <- transExp exp3 e ns
      case t of
        Tnonnull Tbool -> case (a, b, canAssign a b, canAssign b a) of
          (Tnull, Tnonnull x, _, _) -> return(ns, Tnullable x)
          (Tnonnull x, Tnull, _, _) -> return(ns, Tnullable x)
          (_, _, True, _) -> return(ns, a)
          (_, _, _, True) -> return(ns, b)
          otherwise -> error ("Return expressions have different types: " ++ show a ++ " and " ++ show b)
        otherwise -> error ("Wrong expresion inside if statement: " ++ show t)
  Eor exp1 exp2 -> transBoolHelper exp1 exp2 "||" e s 1
  Eand exp1 exp2 -> transBoolHelper exp1 exp2 "&&" e s 1
  Eeq exp1 exp2 -> do
    (ns, a) <- transExp exp1 e s
    (nns, b) <- transExp exp2 e ns
    case (canAssign a b) || (canAssign b a) of
      True -> return(s, Tnonnull Tbool)
      False -> error ("Cannot compare " ++ show a ++ " and " ++ show b)
  Eneq exp1 exp2 -> transExp (Eeq exp1 exp2) e s
  El exp1 exp2 -> transBoolHelper exp1 exp2 "<" e s 2
  Eg exp1 exp2 -> transBoolHelper exp1 exp2 ">" e s 2
  Ele exp1 exp2 -> transBoolHelper exp1 exp2 "<=" e s 2
  Ege exp1 exp2 -> transBoolHelper exp1 exp2 ">=" e s 2
  Eadd exp1 exp2 -> transHelper exp1 exp2 OpAssign2 e s
  Esub exp1 exp2 -> transHelper exp1 exp2 OpAssign3 e s
  Emul exp1 exp2 -> transHelper exp1 exp2 OpAssign4 e s
  Ediv exp1 exp2 -> transHelper exp1 exp2 OpAssign5 e s
  Emod exp1 exp2 -> transHelper exp1 exp2 OpAssign6 e s
  Eneg exp -> do
      (ns, t) <- transExp exp e s
      case t of
        Tnonnull Tint  -> return(ns, t)
        otherwise      -> error ("Cannot negate " ++ show t)
  Elneg exp -> do
      (ns, t) <- transExp exp e s
      case t of
        Tnonnull Tbool  -> return(ns, t)
        otherwise       -> error ("Cannot negate " ++ show t)
  Einc (Evar ident) -> do
      t <- getVal e s ident
      assertNotConst e ident
      case t of
        Tnonnull Tint  -> return(s, t)
        otherwise      -> error ("Cannot increment " ++ show t)
  Einc _ -> error "Only variable shlould be incremented"
  Edec (Evar ident) -> do
      t <- getVal e s ident
      assertNotConst e ident
      case t of
        Tnonnull Tint  -> return(s, t)
        otherwise      -> error ("Cannot decrement " ++ show t)
  Edec _ -> error "Only variable shlould be decremented"
  EPinc (Evar ident) -> do
      t <- getVal e s ident
      assertNotConst e ident
      case t of
        Tnonnull Tint  -> return(s, t)
        otherwise      -> error ("Cannot post-increment " ++ show t)
  EPinc _ -> error "Only variable shlould be post-incremented"
  EPdec (Evar ident) -> do
      t <- getVal e s ident
      assertNotConst e ident
      case t of
        Tnonnull Tint  -> return(s, t)
        otherwise      -> error ("Cannot post-decrement " ++ show t)
  EPdec _ -> error "Only variable shlould be post-decremented"
--  Etupla exps -> do
--      (ns, vs) <- transEtuplaHelper exps e s
--      return(ns, VTupla vs)
  Eint integer -> return(s, Tnonnull Tint)
  Estring string -> return(s, Tnonnull Tstring)
  Etrue -> return(s, Tnonnull Tbool)
  Efalse -> return(s, Tnonnull Tbool)
  Enull -> return(s, Tnull)
--  Ecall functionexp -> failure x
--  Eget ident dimexps -> failure x
--  Elambda args stms -> failure x
--  Ennass exp -> failure x
  Evar ident -> do
      val <- getVal e s ident
      return(s, val)

transOpAssign :: OpAssign -> Type -> Type -> IO(Type)
transOpAssign x a b = case x of
  OpAssign1 -> tryAssign a b
  OpAssign2 -> do
      t <- tryAssign a b
      case t of
        Tnullable Tint    -> return(Tnullable Tint)
        Tnonnull Tint     -> return(Tnonnull Tint)
        Tnullable Tstring -> return(Tnullable Tstring)
        Tnonnull Tstring  -> return(Tnonnull Tstring)
        otherwise         -> error ("Cannot add " ++ show a ++ " to " ++ show b)
  OpAssign3 -> do
      t <- tryAssign a b
      case t of
        Tnullable Tint    -> return(Tnullable Tint)
        Tnonnull Tint     -> return(Tnonnull Tint)
        otherwise         -> error ("Cannot subtrack " ++ show b ++ " from " ++ show a)
  OpAssign4 -> do
      t <- tryAssign a b
      case t of
        Tnullable Tint    -> return(Tnullable Tint)
        Tnonnull Tint     -> return(Tnonnull Tint)
        otherwise         -> error ("Cannot multiply " ++ show a ++ " with " ++ show b)
  OpAssign5 -> do
      t <- tryAssign a b
      case t of
        Tnullable Tint    -> return(Tnullable Tint)
        Tnonnull Tint     -> return(Tnonnull Tint)
        otherwise         -> error ("Cannot divide " ++ show a ++ " by " ++ show b)
  OpAssign6 -> do
      t <- tryAssign a b
      case t of
        Tnullable Tint    -> return(Tnullable Tint)
        Tnonnull Tint     -> return(Tnonnull Tint)
        otherwise         -> error ("Cannot calcule modulation of " ++ show a ++ " by " ++ show b)