module TypeCheck where

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
--    -1 index     -2 howManyLoops     -3 depth     -4 main   -5 return expected   -6 return made
    let s = S (fromList [(-1, Help 1), (-2, Help 0), (-3, Help 0), (-4, Help 0), (-5, HelpRet TRunit), (-6, Help 0)])
    S nsm <- transProg prog e s
    case nsm ! (-4) of
      Help 1 -> return()
      _ -> error "Main was not found"

transProg :: Prog -> Env -> State -> IO State
transProg x e@(E em) s@(S sm) = case x of
  Program [] -> do
      putStrLn ""
      putStrLn ""
      putStrLn ""
      putStrLn (Data.Map.Internal.Debug.showTree em)
      putStrLn (Data.Map.Internal.Debug.showTree sm)
      return s
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
    (Tarray a, Tarray b)  -> canAssign a b
    _                     -> False

canAssign :: Type -> Type -> Bool
canAssign x y = case (x, y) of
    (a, b) | a == b            -> True
    (Tfun _ _,    _)           -> False
    (_,           Tfun _ _)    -> False
    (Tnonnull _,  Tnull)       -> False
    (Tnullable _, Tnull)       -> True
    (Tnullable a, Tnonnull b)  -> canAssignB a b
    (Tnullable a, Tnullable b) -> canAssignB a b
    (Tnonnull a,  Tnonnull b)  -> canAssignB a b
    (Tnonnull _,  Tnullable _) -> False
    _                          -> False

-- try to do: a = b
tryAssign :: Type -> Type -> IO Type
tryAssign a b = if canAssign a b then
    return a
  else
    error ("Cannot assign " ++ show b ++ " to " ++ show a)

-- TODO: gorliwośc ifa usunac
-- TODO: keyword if for run... tablica
alloc :: Env -> State -> Ident -> Bool -> Type -> IO (Env, State)
alloc (E em) (S sm) ident const t = do
    let Help depth = sm ! (-3)
    case ident of
      Ident "main" -> error "Main is keyword"
      _ -> case Data.Map.lookup ident em of
          Just (_, _, d)  | d == depth -> error (show ident ++ " was previously declared in this scope")
          _ -> do
              let Help index = sm ! (-1)
              let ne = E (insert ident (index, const, depth) em)
              let nsm = insert (-1) (Help (index+1)) sm
              let nns = S (insert index  t nsm)
              return(ne, nns)

getIdx :: State -> Integer -> IO Type
getIdx (S sm) index = return(sm ! index)

getVal :: Env -> State -> Ident -> IO Type
getVal (E em) s ident = case Data.Map.lookup ident em of
    Nothing -> error ( show ident ++ " is not defined")
    Just (index, _, _) -> getIdx s index

assertNotConst :: Env -> Ident -> IO ()
assertNotConst e@(E em) ident = do
    let (_, const, _) = em ! ident
    when const $ error "Val cannot be reasigned"

incDepth :: State -> State
incDepth (S sm) = let
    Help depth = sm ! (-3)
    in S (insert (-3) (Help (depth+1)) sm)



-- ----------------------- --
-- D E C L A R A T I O N S --
-- ----------------------- --

declare :: Env -> State -> Ident -> Type -> Bool -> Exp -> IO (Env, State)
declare e s ident a const exp = do
      (ns, b) <- transExp exp e s
      _ <- tryAssign a b
      (ne, nns) <- alloc e ns ident const a
      return(ne, nns)

transDec :: Dec -> Env -> State -> IO (Env, State)
transDec x e s = case x of
  Dfun functiondec -> transFunctionDec functiondec e s
--  Darray arraydec -> transArrayDec arraydec e s
  Dvar ident t exp -> declare e s ident t False exp
  Dval ident type_ exp -> declare e s ident type_ True exp
  Dvarnull ident type_ -> declare e s ident type_ False Enull
  Dvalnull ident type_ -> declare e s ident type_ True Enull

addArgsHelper :: [Arg] -> Env -> State -> IO(Env, State)
addArgsHelper args e s = case args of
    [] -> return(e, s)
    Args ident _type:tail -> do
        (ne, ns) <- alloc e s ident False _type
        addArgsHelper tail ne ns

transFunctionDec :: FunctionDec -> Env -> State -> IO (Env, State)
transFunctionDec x e s@(S sm) = case x of
  FunDec (Ident "main") [] TRunit stms ->
      case sm ! (-4) of
        Help 0 -> do
          let ns@(S nsm) = S (insert (-4) (Help 1) sm)
          let nns = S (insert (-5) (HelpRet TRunit) nsm)
          let nnns = incDepth nns
          (_, _, v) <- doStms stms e nnns
          return (e, ns)
        _ -> error "Main was previously declared"
  FunDec (Ident "main") _ _ _ -> error "Type of main shoud be: () -> Unit"
  FunDec ident args ret stms -> do
      let t = Prelude.map (\(Args _ x) -> x) args
      (xe, xs) <- alloc e s ident True (Tfun t ret)
      (ne, ns) <- addArgsHelper args xe xs
      let nns@(S nnsm) = incDepth ns
      let nnns@(S nnnsm) = S (insert (-5) (HelpRet ret) nnsm)
      let nnnns = S (insert (-6) (Help 0) nnnsm)
      (_, S retSm, v) <- doStms stms ne nnnns
      case (ret, retSm ! (-6)) of
        (TRunit, _) -> return (ne, ns)
        (_, Help 0) -> error "Function should return something"
        orherwise -> return (ne, ns)





-- --------------- --
-- I T E R A B L E --
-- --------------- --

transIterable :: Iterable -> Env -> State -> IO (State, Type)
transIterable x e s = case x of
  Itrange exp1 exp2 -> do
      (ns, t1) <- transExp exp1 e s
      (nns, t2) <- transExp exp2 e ns
      case (t1, t2) of
        (Tnonnull Tint, Tnonnull Tint) -> return (nns, Tnonnull (Tarray (Tnonnull Tint)))
        _ -> error ("Iterable shlould be <integer>..<integer>, not <" ++ show t1 ++ ">..<" ++ show t2 ++ ">")
  Itup exp1 exp2 -> do
      (ns, t1) <- transExp exp1 e s
      (nns, t2) <- transExp exp2 e ns
      case (t1, t2) of
        (Tnonnull Tint, Tnonnull Tint) -> return (nns, Tnonnull (Tarray (Tnonnull Tint)))
        _ -> error ("Iterable shlould be <integer> until <integer>, not <" ++ show t1 ++ "> until <" ++ show t2 ++ ">")
  Itdown exp1 exp2 -> do
      (ns, t1) <- transExp exp1 e s
      (nns, t2) <- transExp exp2 e ns
      case (t1, t2) of
        (Tnonnull Tint, Tnonnull Tint) -> return (nns, Tnonnull (Tarray (Tnonnull Tint)))
        _ -> error ("Iterable shlould be <integer> downTo <integer>, not <" ++ show t1 ++ "> downTo <" ++ show t2 ++ ">")
  Itupst exp1 exp2 exp3 -> do
      (ns, t1) <- transExp exp1 e s
      (nns, t2) <- transExp exp2 e ns
      (nnns, t3) <- transExp exp2 e nns
      case (t1, t2, t3) of
        (Tnonnull Tint, Tnonnull Tint, Tnonnull Tint) -> return (nns, Tnonnull (Tarray (Tnonnull Tint)))
        _ -> error ("Iterable shlould be <integer> until <integer> step <integer>, not <" ++ show t1 ++ "> until <" ++ show t2 ++ "> step <" ++ show t3 ++ ">")
  Itdownst exp1 exp2 exp3 -> do
      (ns, t1) <- transExp exp1 e s
      (nns, t2) <- transExp exp2 e ns
      (nnns, t3) <- transExp exp2 e nns
      case (t1, t2, t3) of
        (Tnonnull Tint, Tnonnull Tint, Tnonnull Tint) -> return (nns, Tnonnull (Tarray (Tnonnull Tint)))
        _ -> error ("Iterable shlould be <integer> downTo <integer> step <integer>, not <" ++ show t1 ++ "> downTo <" ++ show t2 ++ "> step <" ++ show t3 ++ ">")





-- ------------------- --
-- S T A T E M E N T S --
-- ------------------- --

doStmsHelper :: [Stm] -> Env -> State -> RetType -> IO (Env, State, RetType)
doStmsHelper stms e s typ = case stms of
    [] -> return(e, s, typ)
    h:t -> do
        (ne, ns, ntyp) <- transStm h e s
        doStmsHelper t ne ns ntyp

doStms :: [Stm] -> Env -> State -> IO (Env, State, RetType)
doStms stms e s = doStmsHelper stms e s TRunit


transStm :: Stm -> Env -> State -> IO (Env, State, RetType)
transStm x e s@(S sm)  = case x of
  Sdec dec -> do
      (ne, se) <- transDec dec e s
      return(ne, se, TRunit)
  Sexp exp -> do
      (_, t) <- transExp exp e s
      return(e, s, TRtype t)
  Sblock stms -> do
      let ns = incDepth s
      (_, S nnsm, t) <- doStms stms e ns
      case nnsm ! (-6) of
        Help 1 -> return (e, S (insert (-6) (Help 1) sm), t)
        _ -> return (e, s, t)
      return(e, s, t)
  Sfor ident exp stms -> do
      (ns, v) <- transExp exp e s
      case v of
        Tnonnull (Tarray t) -> do
          (ne, nns) <- alloc e ns ident True t
          (_, _, _) <- transStm (Sblock stms) ne nns
          return(e, s, TRunit)
        _ -> error "Loop for can only go through nonull iterable elements"
  Swhile exp stms -> do
      (_, _, _) <- transStm (Sif exp stms) e s
      return(e, s, TRunit)
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
      (ns@(S nsm), t) <- transExp exp e s
      case nsm ! (-5) of
        HelpRet (TRtype x) | canAssign x t -> return(e, S (insert (-6) (Help 1) sm), TRtype t)
        HelpRet x -> error ("Function is expected to return " ++ show x ++ " not " ++ show t)
  Sret -> case sm ! (-5) of
      HelpRet TRunit -> return(e, S (insert (-6) (Help 1) sm), TRunit)
      HelpRet x -> error ("Function is expected to return " ++ show x ++ " not " ++ show TRunit)
  Sif exp stms -> do
      (ns, t) <- transExp exp e s
      case t of
        Tnonnull Tbool -> do
          (_,_,t) <- transStm (Sblock stms) e ns
          return (e, s, t)
        _ -> error ("Wrong expresion inside if statement: " ++ show t)
  Sifelse exp stms1 stms2 -> do
      (ns, t) <- transExp exp e s
      case t of
        Tnonnull Tbool -> do
          let nns = incDepth ns
          (_,S a,t) <- doStms stms1 e nns
          (_,S b,t) <- doStms stms2 e ns
          case (a ! (-6), b ! (-6)) of
            (Help 1,Help 1) -> return (e, S (insert (-6) (Help 1) sm), t)
            _ -> return (e, s, t)
        _ -> error ("Wrong expresion inside if statement: " ++ show t)
  Sprint exp -> do
      (ns, t) <- transExp exp e s
      case t of
        Tnullable Tint    -> return(e, s, TRunit)
        Tnonnull Tint     -> return(e, s, TRunit)
        Tnullable Tstring -> return(e, s, TRunit)
        Tnonnull Tstring  -> return(e, s, TRunit)
        Tnullable Tbool   -> return(e, s, TRunit)
        Tnonnull Tbool    -> return(e, s, TRunit)
        _ -> error ("Cannot print " ++ show t)
  Sprintln exp -> transStm (Sprint exp) e s
--  Snotnull exp stms -> do
--      (ns, v) <- transExp exp e s
--      case v of
--        VNull -> return(e, ns, VUnit)
--        _ -> transStm (Sblock stms) e s





-- ------------------- --
-- E X P R E S I O N S --
-- ------------------- --

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

expToType :: [Exp] -> [Type] -> Env -> State -> IO ()
expToType exps types e s = case (types, exps) of
    ([], []) -> return()
    (a:ttail, exp:etail) -> do
        (ns, b) <- transExp exp e s
        tryAssign a b
        expToType etail ttail e ns


transFunctionExp :: FunctionExp -> Env -> State -> IO (State, Type)
transFunctionExp x e s = case x of
  FunCall ident exps -> do
      Tfun t ret <- getVal e s ident
      case (length t, length exps) of
          (expected, passed) | expected == passed -> do
              expToType exps t e s
              case ret of
                TRunit -> return(s, Tunit)
                TRtype x -> return(s,x)
                             | otherwise  -> error ("Passed " ++ show passed ++ " arguments, when " ++ show ident ++ " expects " ++ show expected)

transEtuplaHelper :: [Exp] -> Env -> State -> IO (State, [Type])
transEtuplaHelper exps e s = case exps of
  [] -> return(s, [])
  h:tail -> do
      (ns, t) <- transExp h e s
      (nns, ts) <- transEtuplaHelper tail e ns
      return(nns, t:ts)

transGetExp :: Type -> [DimExp] -> Env -> State -> IO (State, Type)
transGetExp t dims e s = case (t, dims) of
    (_, []) -> return (s, t)
    (Tnonnull (Tarray nt), Dim exp:tail) -> do
        (ns, _type) <- transExp exp e s
        case _type of
            Tnonnull Tint -> transGetExp nt tail e ns
            _ -> error ("Index of array should be nonnullable Integer, not " ++ show _type)
    _ -> error (show t ++ " is not a nonnullable array")

transHelper :: Exp -> Exp -> OpAssign -> Env -> State -> IO (State, Type)
transHelper exp1 exp2 op e s = do
    (ns, a) <- transExp exp1 e s
    (nns, b) <- transExp exp2 e ns
    t <- transOpAssign op a b
    return(nns, t)

transBoolHelper :: Exp -> Exp -> String -> Env -> State -> Integer -> IO (State, Type)
transBoolHelper exp1 exp2 op e s boolInt = do
    (ns, a) <- transExp exp1 e s
    (nns, b) <- transExp exp2 e ns
    case (a, b, boolInt) of
      (Tnonnull Tint, Tnonnull Tint, 2)   -> return(nns, Tnonnull Tbool)
      (Tnonnull Tbool, Tnonnull Tbool, 1) -> return(nns, Tnonnull Tbool)
      _ -> error ("Cannot perform " ++ op ++ " on " ++ show a ++ " and " ++ show b)


transExp :: Exp -> Env -> State -> IO (State, Type)
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
          _ -> error ("Return expressions have different types: " ++ show a ++ " and " ++ show b)
        _ -> error ("Wrong expresion inside if statement: " ++ show t)
  Eor exp1 exp2 -> transBoolHelper exp1 exp2 "||" e s 1
  Eand exp1 exp2 -> transBoolHelper exp1 exp2 "&&" e s 1
  Eeq exp1 exp2 -> do
    (ns, a) <- transExp exp1 e s
    (nns, b) <- transExp exp2 e ns
    if canAssign a b || canAssign b a then
      return(s, Tnonnull Tbool)
    else
      error ("Cannot compare " ++ show a ++ " and " ++ show b)
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
        _              -> error ("Cannot negate " ++ show t)
  Elneg exp -> do
      (ns, t) <- transExp exp e s
      case t of
        Tnonnull Tbool  -> return(ns, t)
        _               -> error ("Cannot negate " ++ show t)
  Einc (Evar ident) -> do
      t <- getVal e s ident
      assertNotConst e ident
      case t of
        Tnonnull Tint  -> return(s, t)
        _              -> error ("Cannot increment " ++ show t)
  Einc _ -> error "Only variable shlould be incremented"
  Edec (Evar ident) -> do
      t <- getVal e s ident
      assertNotConst e ident
      case t of
        Tnonnull Tint  -> return(s, t)
        _              -> error ("Cannot decrement " ++ show t)
  Edec _ -> error "Only variable shlould be decremented"
  EPinc (Evar ident) -> do
      t <- getVal e s ident
      assertNotConst e ident
      case t of
        Tnonnull Tint  -> return(s, t)
        _              -> error ("Cannot post-increment " ++ show t)
  EPinc _ -> error "Only variable shlould be post-incremented"
  EPdec (Evar ident) -> do
      t <- getVal e s ident
      assertNotConst e ident
      case t of
        Tnonnull Tint  -> return(s, t)
        _              -> error ("Cannot post-decrement " ++ show t)
  EPdec _ -> error "Only variable shlould be post-decremented"
  Eiter iterable -> transIterable iterable e s
--  Earray (Eint size) exp -> failure x
  Etupla exps -> do
      (ns, list) <- transEtuplaHelper exps e s
      return(ns, Tnonnull (Ttupla list))
  Eint integer      -> return(s, Tnonnull Tint)
  Estring string    -> return(s, Tnonnull Tstring)
  Etrue             -> return(s, Tnonnull Tbool)
  Efalse            -> return(s, Tnonnull Tbool)
  Enull             -> return(s, Tnull)
  Ecall functionexp -> transFunctionExp functionexp e s
  Eget ident dimexps -> do
      t <- getVal e s ident
      transGetExp t dimexps e s
--  Elambda args stms -> failure x
--  Ennass exp -> failure x
  Evar ident -> do
      val <- getVal e s ident
      return(s, val)

transOpAssign :: OpAssign -> Type -> Type -> IO Type
transOpAssign x a b = case x of
  OpAssign1 -> tryAssign a b
  OpAssign2 -> do
      t <- tryAssign a b
      t <- tryAssign a b
      case t of
        Tnullable Tint    -> return(Tnullable Tint)
        Tnonnull Tint     -> return(Tnonnull Tint)
        Tnullable Tstring -> return(Tnullable Tstring)
        Tnonnull Tstring  -> return(Tnonnull Tstring)
        _                 -> error ("Cannot add " ++ show a ++ " to " ++ show b)
  OpAssign3 -> do
      t <- tryAssign a b
      case t of
        Tnullable Tint    -> return(Tnullable Tint)
        Tnonnull Tint     -> return(Tnonnull Tint)
        _                 -> error ("Cannot subtrack " ++ show b ++ " from " ++ show a)
  OpAssign4 -> do
      t <- tryAssign a b
      case t of
        Tnullable Tint    -> return(Tnullable Tint)
        Tnonnull Tint     -> return(Tnonnull Tint)
        _                 -> error ("Cannot multiply " ++ show a ++ " with " ++ show b)
  OpAssign5 -> do
      t <- tryAssign a b
      case t of
        Tnullable Tint    -> return(Tnullable Tint)
        Tnonnull Tint     -> return(Tnonnull Tint)
        _                 -> error ("Cannot divide " ++ show a ++ " by " ++ show b)
  OpAssign6 -> do
      t <- tryAssign a b
      case t of
        Tnullable Tint    -> return(Tnullable Tint)
        Tnonnull Tint     -> return(Tnonnull Tint)
        _                 -> error ("Cannot calcule modulation of " ++ show a ++ " by " ++ show b)