-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State
import qualified Data.Map as Map

-- -- for debug!
-- import qualified Debug.Trace as Trace
-- -- end of for debug!

data Context = Context {
  -- 可以用某种方式定义上下文，用于记录变量绑定状态
  runContext :: [(String, Type)],
  eDataTypeMap :: [(String, Type)],
  adtsMap :: [(String, [Type])],
  adtsTypeMap :: [(String, Type)]
} deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isBool2 :: Expr -> Expr -> ContextState Type
isBool2 e1 e2 = do
  et1 <- eval e1
  et2 <- eval e2
  case (et1, et2) of
    (TBool, TBool) -> return TBool
    _ -> lift Nothing

isInt2 :: Expr -> Expr -> ContextState Type
isInt2 e1 e2 = do
  et1 <- eval e1
  et2 <- eval e2
  case (et1, et2) of
    (TInt, TInt) -> return TInt
    _ -> lift Nothing

isBIC2 :: Expr -> Expr -> ContextState Type
isBIC2 e1 e2 = do
  et1 <- eval e1
  et2 <- eval e2
  case (et1, et2) of
    (TBool, TBool) -> return TBool
    (TInt, TInt) -> return TBool
    (TChar, TChar) -> return TBool
    _ -> lift Nothing

isIC2 :: Expr -> Expr -> ContextState Type
isIC2 e1 e2 = do
  et1 <- eval e1
  et2 <- eval e2
  case (et1, et2) of
    (TInt, TInt) -> return TBool
    (TChar, TChar) -> return TBool
    _ -> lift Nothing

isIf3 :: Expr -> Expr -> Expr -> ContextState Type
isIf3 eif e1 e2 = do
  etif <- eval eif
  et1 <- eval e1
  et2 <- eval e2
  case (etif, et1==et2) of
    (TBool, True) -> return et1
    _ -> lift Nothing

checkType e1 ty = do
  tGot <- eval e1
  if tGot == ty then return () else lift Nothing

pData2Var :: Pattern -> Type -> ContextState [(String,Type)]
pData2Var (PData cons ps) t = do
  originContext <- get
  case (Map.fromList (adtsMap originContext) Map.!? cons) of
    Just ts -> patterns2Var ps ts

patterns2Var :: [Pattern] -> [Type] -> ContextState [(String,Type)]
patterns2Var [] [] = return []
patterns2Var (p:ps) (t:ts) = do
  vP <- pattern2Var p t
  vPs <- patterns2Var ps ts
  return (vP ++ vPs)

pattern2Var :: Pattern -> Type -> ContextState [(String,Type)]
pattern2Var p t = do
  case p of
    PVar vName -> return [(vName,t)] 
    PData cons ps -> pData2Var p t
    _ -> return []

patternExpr2Type :: Pattern -> Type -> Expr -> ContextState Type
patternExpr2Type p pt expr = do
  vars <- pattern2Var p pt
  localVars vars (eval expr)

patternExprTypes :: [(Pattern,Expr)] -> Type -> ContextState Type
patternExprTypes ((p,expr):[]) pt = do
  t <- patternExpr2Type p pt expr
  return t
patternExprTypes ((p,expr):pes) pt = do
  t1 <- patternExpr2Type p pt expr
  t2 <- patternExprTypes pes pt
  if t1 == t2 
    then return t1
    else lift Nothing 

localVars :: [(String,Type)] -> ContextState Type -> ContextState Type
localVars [] op = op
localVars ((vName,vt):vs) op = do
  originContext <- get
  put $ insertIntoContext originContext vName vt
  result <- localVars vs op
  put originContext
  return result

pDataType :: Pattern -> ContextState Type
pDataType (PData s ps) = do
  originContext <- get
  case (Map.fromList (adtsMap originContext) Map.!? s) of
    Just ts -> patternsType ps ts >> case (Map.fromList (eDataTypeMap originContext) Map.!? s) of
      Just t -> return t
      Nothing -> lift Nothing
    Nothing -> lift Nothing

patternType :: Pattern -> Type -> ContextState Type
patternType p t = do
  case p of
    PBoolLit b -> 
      if t == TBool then return TBool else lift Nothing
    PIntLit i -> 
      if t == TInt then return TInt else lift Nothing
    PCharLit c -> 
      if t == TChar then return TChar else lift Nothing
    PVar s -> do
      originContext <- get
      case (Map.fromList (runContext originContext) Map.!? s) of
        Just et -> return et
        Nothing -> return t
    PData cons ps -> do
      originContext <- get
      case (Map.fromList (eDataTypeMap originContext) Map.!? cons) of
        Nothing -> lift Nothing
        Just dt -> 
          if dt == t then pDataType (PData cons ps) else lift Nothing

patternsType :: [Pattern] -> [Type] -> ContextState Type
patternsType [] [] = return TBool
patternsType (p:ps) (t:ts) = do 
  pType <- (patternType p t)
  psType <- (patternsType ps ts)
  return psType

eDataTypes :: [Expr] -> [Type] -> ContextState [Type]
eDataTypes [] [] = return []
eDataTypes (e:es) (t:ts) = do
  et <- eDataType e t
  ests <- eDataTypes es ts
  return (et : ests)

eDataType :: Expr -> Type -> ContextState Type
eDataType e t = do
  et <- eval e
  if et == t then return et else lift Nothing

insertIntoContext originContext pn pt = (Context  (Map.toList $ Map.insert pn pt (Map.fromList $ runContext originContext)) (eDataTypeMap originContext) (adtsMap originContext) (adtsTypeMap originContext))

eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (EIntLit _) = return TInt
eval (ECharLit _) = return TChar

eval (ENot e) = isBool e >> return TBool
eval (EAnd e1 e2) = isBool2 e1 e2 >> return TBool
eval (EOr e1 e2) = isBool2 e1 e2 >> return TBool

eval (EAdd e1 e2) = isInt2 e1 e2 >> return TInt
eval (ESub e1 e2) = isInt2 e1 e2 >> return TInt
eval (EMul e1 e2) = isInt2 e1 e2 >> return TInt
eval (EDiv e1 e2) = isInt2 e1 e2 >> return TInt
eval (EMod e1 e2) = isInt2 e1 e2 >> return TInt

eval (EEq e1 e2) = isBIC2 e1 e2 >> return TBool
eval (ENeq e1 e2) = isBIC2 e1 e2 >> return TBool
eval (ELt e1 e2) = isIC2 e1 e2 >> return TBool
eval (EGt e1 e2) = isIC2 e1 e2 >> return TBool
eval (ELe e1 e2) = isIC2 e1 e2 >> return TBool
eval (EGe e1 e2) = isIC2 e1 e2 >> return TBool

eval (EIf e1 e2 e3) = isIf3 e1 e2 e3

eval (ELambda (pn, pt) e) = do
  originContext <- get
  -- -- for debug!
  -- Trace.trace ("\nELambda  context: " ++ (show originContext) ++ "\n   pn = " ++ (show pn) ++ "\n && pt = " ++ (show pt) ++ "\n && e = " ++ (show e)) $ return TInt
  -- -- end of for debug!
  put $ insertIntoContext originContext pn pt
  eType <- eval e
  return $ TArrow pt eType

eval (ELet (s, es) e) = do
  originContext <- get
  -- -- for debug!
  -- Trace.trace ("\nELet  context: " ++ (show originContext) ++ "\n   s = " ++ (show s) ++ "\n && es = " ++ (show es) ++ "\n && e = " ++ (show e)) $ return TInt
  -- -- end of for debug!
  esType <- eval es
  put $ insertIntoContext originContext s esType
  eType <- eval e
  put originContext  -- 因为let只是临时绑定
  return eType

eval (ELetRec f (x, tx) (e1, ty) e2) = do
  originContext <- get
  -- -- for debug!
  -- Trace.trace ("\nELetRec  context: " ++ (show originContext) ++ "\n   f = " ++ (show f) ++ "\n && x = " ++ (show x) ++ "\n && tx = " ++ (show tx) ++ "\n && e1 = " ++ (show e1) ++ "\n && ty = " ++ (show ty) ++ "\n && e2 = " ++ (show e2)) $ return TInt
  -- -- end of for debug!
  put $ insertIntoContext originContext x tx
  put $ insertIntoContext (insertIntoContext originContext x tx) f (TArrow tx ty)
  checkType e1 ty
  eType <- eval e2
  put originContext  -- 因为let只是临时绑定
  return eType

eval (EApply e1 e2) = do
  typeE1 <- eval e1
  typeE2 <- eval e2
  -- -- for debug!
  -- originContext <- get
  -- Trace.trace ("\nEApply  context: " ++ (show originContext) ++ "\n   typeE1 = " ++ (show typeE1) ++ "\n && typeE2 = " ++ (show typeE2) ++ "\n && e1 = " ++ (show e1) ++ "\n && e2 = " ++ (show e2)) $ return TInt
  -- -- end of for debug!
  case typeE1 of
    (TArrow t0 t1) | t0 == typeE2 -> return t1
    _ -> lift Nothing

eval (EVar s) = do
  originContext <- get
  -- -- for debug!
  -- Trace.trace ("\nEVar  context: " ++ (show originContext) ++ "\n   s = " ++ (show s)) $ return TInt
  -- -- end of for debug!
  case (Map.fromList (runContext originContext)) Map.!? s of
    (Just a) -> return a
    _ -> case (Map.fromList (adtsTypeMap originContext) Map.!? s) of
      Just a -> return a
      _ -> lift Nothing

eval (ECase e ps) = do
  et <- eval e
  patternsType (fst $ unzip ps) (replicate (length ps) et)
  rt <- patternExprTypes ps et
  return rt

eval (EData cons es) = do
  originContext <- get
  case (Map.fromList (eDataTypeMap originContext) Map.!? cons) of
    Just cont ->
      case (Map.fromList (adtsMap originContext) Map.!? cons) of
        Just ts -> 
          if (length es) == (length ts)
            then do
              eDataTypes es ts
              return $ cont
            else lift Nothing
        Nothing -> lift Nothing
    Nothing -> lift Nothing 

-- eval _ = do
--   lift Nothing

geteDataTypeMap = foldl (\acc (ADT name xs) -> (foldl (\acc x -> (x, TData name) : acc) [] (fst $ unzip xs)) ++ acc) []

getAdtsMap = foldl (\acc (ADT name xs) -> xs ++ acc) []

getConsFuncType tname [] = TData tname
getConsFuncType tname (t:ts) = TArrow t (getConsFuncType tname ts)
getAdtsType (ADT tname []) = []
getAdtsType (ADT tname ((name,ts):cs)) = (name,(getConsFuncType tname ts)):(getAdtsType (ADT tname cs))
getAdtsTypeMap [] = []
getAdtsTypeMap (adt:adts) = (getAdtsType adt) ++ (getAdtsTypeMap adts)

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  (Context []  (geteDataTypeMap adts) (getAdtsMap adts) (getAdtsTypeMap adts)) -- 可以用某种方式定义上下文，用于记录变量绑定状态