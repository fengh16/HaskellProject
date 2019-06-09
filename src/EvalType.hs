-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
                       }
  deriving (Show, Eq)

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

-- evalTypeInternal :: Expr -> ContextState Type
-- evalTypeInternal e1 e2 e3 = do
--   et1 <- eval e1
--   case et1 of
--     (TData s1) ->
--       Context

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
eval (ELambda (pn, pt) e1) = undefined

eval _ = undefined


evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context {  } -- 可以用某种方式定义上下文，用于记录变量绑定状态
