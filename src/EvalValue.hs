-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State

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

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit b) = return $ VInt b
eval (ECharLit b) = return $ VChar b

eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)
eval (EAnd e1 e2) = do
  b1 <- getBool e1
  b2 <- getBool e2
  return (VBool (b1 && b2))
eval (EOr e1 e2) = do
  b1 <- getBool e1
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
  evif <- eval eif
  (ev1, ev2) <- checkTwoEqual e1 e2
  case evif of
    (VBool True) -> return ev1
    (VBool False) -> return ev2
    _ -> lift Nothing
-- ... more
eval _ = undefined

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context [] -- 可以用某种方式定义上下文，用于记录变量绑定状态


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
