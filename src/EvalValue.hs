-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State
import qualified Data.Map as Map

-- for debug!
import qualified Debug.Trace as Trace
-- end of for debug!

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  -- ... more
  deriving (Show, Eq)

data Context = Context {
  -- 可以用某种方式定义上下文，用于记录变量绑定状态
  runContext :: [(String, Expr)]
} deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

getTwoBIC e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VBool b1, VBool b2) -> return (compare b1 b2)
    (VInt b1, VInt b2) -> return (compare b1 b2)
    (VChar b1, VChar b2) -> return (compare b1 b2)
    _ -> lift Nothing

getTwoIC e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt b2) -> return (compare b1 b2)
    (VChar b1, VChar b2) -> return (compare b1 b2)
    _ -> lift Nothing

checkTwoEqual e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VBool _, VBool _) -> return (ev1, ev2)
    (VInt _, VInt _) -> return (ev1, ev2)
    (VChar _, VChar _) -> return (ev1, ev2)
    _ -> lift Nothing

getTwoInt e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt b2) -> return (b1, b2)
    _ -> lift Nothing

insertIntoContext originContext pn pt = Context $ Map.toList $ Map.insert pn pt (Map.fromList $ runContext originContext)

doApply :: Expr -> Expr -> ContextState Expr
doApply e e1 = do
  originContext <- get
  -- for debug!
  Trace.trace ("\ndoEApply  context: " ++ (show originContext) ++ "\n   e1 = " ++ (show e1) ++ "\n && e = " ++ (show e)) $ return (VBool False)
  -- end of for debug!
  func <- checkEVarELambda e
  case func of
    (ELambda (x, t) e2) -> do
      put $ Context $ Map.toList $ Map.delete x (Map.fromList (runContext originContext))
      parsedE1 <- simplifyExpr e1
      put $ insertIntoContext originContext x parsedE1
      -- for debug!
      t <- get
      Trace.trace ("\ndoEApply2  context: " ++ (show t) ++ "\n") $ return (VBool False)
      -- end of for debug!
      ans <- simplifyExpr e2
      put originContext
      return ans
    _ -> lift Nothing

checkEVarELambda :: Expr -> ContextState Expr
checkEVarELambda (EVar s) = do
  originContext <- get
  -- -- for debug!
  -- Trace.trace ("\ngetEVar  context: " ++ (show originContext) ++ "\n   s = " ++ (show s)) $ return (VBool False)
  -- -- end of for debug!
  case (Map.fromList (runContext originContext)) Map.!? s of
    (Just a) -> checkEVarELambda a
    _ -> lift Nothing
checkEVarELambda (EApply m e) = do
  originContext <- get
  func <- checkEVarELambda m
  case func of
    (ELambda (pn, pt) e) -> do
      put $ Context $ Map.toList $ Map.delete pn (Map.fromList (runContext originContext))
    _ -> lift Nothing
  ans <- doApply m e
  put originContext
  return ans
checkEVarELambda e@(ELambda _ _) = do
  return e
checkEVarELambda _ = do
  lift Nothing

parseVar :: Expr -> ContextState Expr
parseVar (EVar s) = do
  originContext <- get
  case (Map.fromList (runContext originContext)) Map.!? s of
    (Just a) -> return a
    _ -> return (EVar s)
parseVar _ = do
  lift Nothing

checkBase a =
  case a of
    (EBoolLit _) -> True
    (EIntLit _) -> True
    (ECharLit _) -> True
    _ -> False

simplifyExpr :: Expr -> ContextState Expr
simplifyExpr a@(EVar e) = do
  parseVar a
simplifyExpr (ENot e) = do
  ans <- simplifyExpr e
  if checkBase ans
    then do
      a <- eval (ENot ans)
      valueToExpr a
    else return (ENot ans)
simplifyExpr (EAnd e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (EAnd ans1 ans2)
      valueToExpr a
    else return (EAnd ans1 ans2)
simplifyExpr (EOr e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (EOr ans1 ans2)
      valueToExpr a
    else return (EOr ans1 ans2)
simplifyExpr (EAdd e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (EAdd ans1 ans2)
      valueToExpr a
    else return (EAdd ans1 ans2)
simplifyExpr (ESub e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (ESub ans1 ans2)
      valueToExpr a
    else return (ESub ans1 ans2)
simplifyExpr (EMul e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (EMul ans1 ans2)
      valueToExpr a
    else return (EMul ans1 ans2)
simplifyExpr (EDiv e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (EDiv ans1 ans2)
      valueToExpr a
    else return (EDiv ans1 ans2)
simplifyExpr (EMod e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (EMod ans1 ans2)
      valueToExpr a
    else return (EMod ans1 ans2)
simplifyExpr (EEq e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (EEq ans1 ans2)
      valueToExpr a
    else return (EEq ans1 ans2)
simplifyExpr (ENeq e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (ENeq ans1 ans2)
      valueToExpr a
    else return (ENeq ans1 ans2)
simplifyExpr (ELt e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (ELt ans1 ans2)
      valueToExpr a
    else return (ELt ans1 ans2)
simplifyExpr (EGt e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (EGt ans1 ans2)
      valueToExpr a
    else return (EGt ans1 ans2)
simplifyExpr (ELe e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (ELe ans1 ans2)
      valueToExpr a
    else return (ELe ans1 ans2)
simplifyExpr (EGe e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (EGe ans1 ans2)
      valueToExpr a
    else return (EGe ans1 ans2)
simplifyExpr (EIf eif e1 e2) = do
  ansif <- simplifyExpr eif
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ansif && checkBase ans1 && checkBase ans2
    then do
      a <- eval (EIf ansif ans1 ans2)
      valueToExpr a
    else return (EIf ansif ans1 ans2)
simplifyExpr (ELambda (pn, pt) e) = do
  originContext <- get
  put $ Context $ Map.toList $ Map.delete pn (Map.fromList (runContext originContext))
  ans <- simplifyExpr e
  put originContext
  return (ELambda (pn, pt) ans)
simplifyExpr (ELet (n, e1) e2) = do
  originContext <- get
  put $ Context $ Map.toList $ Map.delete n (Map.fromList (runContext originContext))
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (ELet (n, ans1) ans2)
      put originContext
      valueToExpr a
    else do
      put originContext
      return (ELet (n, ans1) ans2)
simplifyExpr (ELetRec f (x, tx) (e1, ty) e2) = do
  originContext <- get
  put $ Context $ Map.toList $ Map.delete x $ Map.delete f (Map.fromList (runContext originContext))
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  if checkBase ans1 && checkBase ans2
    then do
      a <- eval (ELetRec f (x, tx) (ans1, ty) ans2)
      put originContext
      valueToExpr a
    else do
      put originContext
      return (ELetRec f (x, tx) (ans1, ty) ans2)
simplifyExpr (EApply e1 e2) = do
  ans1 <- simplifyExpr e1
  ans2 <- simplifyExpr e2
  return (EApply ans1 ans2)
simplifyExpr a = do
  return a

valueToExpr :: Value -> ContextState Expr
valueToExpr (VBool b) = return $ EBoolLit b
valueToExpr (VInt b) = return $ EIntLit b
valueToExpr (VChar b) = return $ ECharLit b

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit b) = return $ VInt b
eval (ECharLit b) = return $ VChar b

eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)
eval (EAnd e1 e2) = do
  b1 <- getBool e1
  if b1 == False
    then do
      return (VBool False) 
    else do
      b2 <- getBool e2
      return (VBool (b1 && b2))
eval (EOr e1 e2) = do
  b1 <- getBool e1
  if b1 == True 
    then do 
      return (VBool True) 
    else do
      b2 <- getBool e2
      return (VBool (b1 || b2))

eval (EAdd e1 e2) = getTwoInt e1 e2 >>= \(ev1, ev2) -> return (VInt (ev1 + ev2))
eval (ESub e1 e2) = getTwoInt e1 e2 >>= \(ev1, ev2) -> return (VInt (ev1 - ev2))
eval (EMul e1 e2) = getTwoInt e1 e2 >>= \(ev1, ev2) -> return (VInt (ev1 * ev2))
eval (EDiv e1 e2) = getTwoInt e1 e2 >>= \(ev1, ev2) -> (
    if ev2 /= 0 then return (VInt (div ev1 ev2)) else lift Nothing
  )
eval (EMod e1 e2) = getTwoInt e1 e2 >>= \(ev1, ev2) -> (
    if ev2 /= 0 then return (VInt (mod ev1 ev2)) else lift Nothing
  )

eval (EEq e1 e2) = getTwoBIC e1 e2 >>= \cmp -> return (VBool (cmp == EQ))
eval (ENeq e1 e2) = getTwoBIC e1 e2 >>= \cmp -> return (VBool (cmp /= EQ))
eval (ELe e1 e2) = getTwoIC e1 e2 >>= \cmp -> return (VBool (cmp == EQ || cmp == LT))
eval (ELt e1 e2) = getTwoIC e1 e2 >>= \cmp -> return (VBool (cmp == LT))
eval (EGe e1 e2) = getTwoIC e1 e2 >>= \cmp -> return (VBool (cmp == EQ || cmp == GT))
eval (EGt e1 e2) = getTwoIC e1 e2 >>= \cmp -> return (VBool (cmp == GT))

eval (EIf eif e1 e2) = do
  -- for debug!
  Trace.trace ("\nEIf\n   eif = " ++ (show eif) ++ "\n && e1 = " ++ (show e1) ++ "\n && e2 = " ++ (show e2)) $ return (VBool False)
  -- end of for debug!
  evif <- eval eif
  case evif of
    (VBool True) -> do
      ev1 <- eval e1
      return ev1
    (VBool False) -> do
      ev2 <- eval e2
      return ev2
    _ -> lift Nothing

eval (ELet (s, es) e) = do
  originContext <- get
  -- for debug!
  Trace.trace ("\nELet  context: " ++ (show originContext) ++ "\n   s = " ++ (show s) ++ "\n && es = " ++ (show es) ++ "\n && e = " ++ (show e)) $ return (VBool False)
  -- end of for debug!
  parsedES <- simplifyExpr es
  put $ insertIntoContext originContext s parsedES
  -- for debug!
  k <- get
  Trace.trace ("\nELet  nowcontext: " ++ (show k)) $ return (VBool False)
  -- end of for debug!
  eVal <- eval e
  put originContext  -- 因为let只是临时绑定
  return eVal
eval (ELetRec f (x, tx) (e1, ty) e2) = do
  originContext <- get
  -- for debug!
  Trace.trace ("\nELetRec  context: " ++ (show originContext) ++ "\n   f = " ++ (show f) ++ "\n && x = " ++ (show x) ++ "\n && tx = " ++ (show tx) ++ "\n && e1 = " ++ (show e1) ++ "\n && ty = " ++ (show ty) ++ "\n && e2 = " ++ (show e2)) $ return (VBool False)
  -- end of for debug!
  parsedE1 <- simplifyExpr e1
  put $ insertIntoContext originContext f (ELambda (x, tx) parsedE1)
  eVal <- eval e2
  put originContext  -- 因为let只是临时绑定
  return eVal
eval (EApply e e1) = do
  originContext <- get
  -- for debug!
  Trace.trace ("\nEApply  context: " ++ (show originContext) ++ "\n   e1 = " ++ (show e1) ++ "\n && e = " ++ (show e)) $ return (VBool False)
  -- end of for debug!
  func <- checkEVarELambda e
  case func of
    (ELambda (x, t) e2) -> do
      evaledE1 <- eval e1
      parsedE1 <- valueToExpr evaledE1
      put $ insertIntoContext originContext x parsedE1
      ans <- eval e2
      put originContext  -- 因为let只是临时绑定
      return ans
    _ -> lift Nothing

eval (EVar s) = do
  originContext <- get
  -- for debug!
  Trace.trace ("\nEVar  context: " ++ (show originContext) ++ "\n   s = " ++ (show s)) $ return (VBool False)
  -- end of for debug!
  case (Map.fromList (runContext originContext)) Map.!? s of
    (Just a) -> eval a
    _ -> lift Nothing
    
eval _ = do
  lift Nothing

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context [] -- 可以用某种方式定义上下文，用于记录变量绑定状态


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
