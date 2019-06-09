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

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
                          } deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

getAnd :: Expr -> Expr -> ContextState Bool
getAnd e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VBool False, _) -> return False
    (VBool True, VBool b2) -> return b2
    _ -> lift Nothing

getOr :: Expr -> Expr -> ContextState Bool
getOr e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VBool True, _) -> return True
    (VBool False, VBool b2) -> return b2
    _ -> lift Nothing

getAdd :: Expr -> Expr -> ContextState Int
getAdd e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt b2) -> return (b1 + b2)
    _ -> lift Nothing

getSub :: Expr -> Expr -> ContextState Int
getSub e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt b2) -> return (b1 - b2)
    _ -> lift Nothing

getMul :: Expr -> Expr -> ContextState Int
getMul e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt b2) -> return (b1 * b2)
    _ -> lift Nothing

getDiv :: Expr -> Expr -> ContextState Int
getDiv e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt 0) -> lift Nothing
    (VInt b1, VInt b2) -> return (b1 `div` b2)
    _ -> lift Nothing

getMod :: Expr -> Expr -> ContextState Int
getMod e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt 0) -> lift Nothing
    (VInt b1, VInt b2) -> return (b1 `mod` b2)
    _ -> lift Nothing

getEq :: Expr -> Expr -> ContextState Bool
getEq e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VBool b1, VBool b2) -> return (b1 == b2)
    (VInt b1, VInt b2) -> return (b1 == b2)
    (VChar b1, VChar b2) -> return (b1 == b2)
    _ -> lift Nothing

getNeq :: Expr -> Expr -> ContextState Bool
getNeq e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VBool b1, VBool b2) -> return (b1 /= b2)
    (VInt b1, VInt b2) -> return (b1 /= b2)
    (VChar b1, VChar b2) -> return (b1 /= b2)
    _ -> lift Nothing

getLe :: Expr -> Expr -> ContextState Bool
getLe e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt b2) -> return (b1 <= b2)
    (VChar b1, VChar b2) -> return (b1 <= b2)
    _ -> lift Nothing

getLt :: Expr -> Expr -> ContextState Bool
getLt e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt b2) -> return (b1 < b2)
    (VChar b1, VChar b2) -> return (b1 < b2)
    _ -> lift Nothing

getGe :: Expr -> Expr -> ContextState Bool
getGe e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt b2) -> return (b1 >= b2)
    (VChar b1, VChar b2) -> return (b1 >= b2)
    _ -> lift Nothing

getGt :: Expr -> Expr -> ContextState Bool
getGt e1 e2 = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (VInt b1, VInt b2) -> return (b1 > b2)
    (VChar b1, VChar b2) -> return (b1 > b2)
    _ -> lift Nothing

getIfAns :: Expr -> Expr -> Expr -> ContextState Value
getIfAns eif e1 e2 = do
  evif <- eval eif
  case (evif) of
    (VBool True) -> eval e1
    (VBool False) -> eval e2
    _ -> lift Nothing

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit b) = return $ VInt b
eval (ECharLit b) = return $ VChar b
eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)
eval (EAnd e1 e2) = getAnd e1 e2 >>= \b -> return (VBool b)
eval (EOr e1 e2) = getOr e1 e2 >>= \b -> return (VBool b)
eval (EAdd e1 e2) = getAdd e1 e2 >>= \b -> return (VInt b)
eval (ESub e1 e2) = getSub e1 e2 >>= \b -> return (VInt b)
eval (EMul e1 e2) = getMul e1 e2 >>= \b -> return (VInt b)
eval (EDiv e1 e2) = getDiv e1 e2 >>= \b -> return (VInt b)
eval (EMod e1 e2) = getMod e1 e2 >>= \b -> return (VInt b)
eval (EEq e1 e2) = getEq e1 e2 >>= \b -> return (VBool b)
eval (ENeq e1 e2) = getNeq e1 e2 >>= \b -> return (VBool b)
eval (ELe e1 e2) = getLe e1 e2 >>= \b -> return (VBool b)
eval (ELt e1 e2) = getLt e1 e2 >>= \b -> return (VBool b)
eval (EGe e1 e2) = getGe e1 e2 >>= \b -> return (VBool b)
eval (EGt e1 e2) = getGt e1 e2 >>= \b -> return (VBool b)
eval (EIf eif e1 e2) = getIfAns eif e1 e2
-- ... more
eval _ = undefined

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context {  } -- 可以用某种方式定义上下文，用于记录变量绑定状态


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
