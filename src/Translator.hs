module Translator where

import AST

eval :: Expr -> IO()
eval (EBoolLit b) = 
    case b of
        False -> do
            putStr "false"
        True -> do
            putStr "true"

eval (EIntLit b) = putStr (show b)
eval (ECharLit b) = putStr (show b)
eval (EVar b) = putStr $ id b

eval (ENot e) = do
    putStr "("
    putStr "!"
    eval e
    putStr ")"
eval (EAnd e1 e2) = do
    putStr "("
    eval e1
    putStr " && "
    eval e2
    putStr ")"

eval (EOr e1 e2) = do
    putStr "("
    eval e1
    putStr " || "
    eval e2
    putStr ")"
eval (EAdd e1 e2) = do
    putStr "("
    eval e1
    putStr " + "
    eval e2
    putStr ")"

eval (ESub e1 e2) = do
    putStr "("
    eval e1
    putStr " - "
    eval e2
    putStr ")"
eval (EMul e1 e2) = do
    putStr "("
    eval e1
    putStr " * "
    eval e2
    putStr ")"
eval (EDiv e1 e2) = do
    putStr "Math.floor("
    eval e1
    putStr " / "
    eval e2
    putStr ")"
eval (EMod e1 e2) = do
    putStr "("
    eval e1
    putStr " % "
    eval e2
    putStr ")"

eval (EEq e1 e2) = do
    putStr "("
    eval e1
    putStr " == "
    eval e2
    putStr ")"

eval (ENeq e1 e2) = do
    putStr "("
    eval e1
    putStr " != "
    eval e2
    putStr ")"

eval (ELe e1 e2) = do
    putStr "("
    eval e1
    putStr " <= "
    eval e2
    putStr ")"
eval (ELt e1 e2) = do
    putStr "("
    eval e1
    putStr " < "
    eval e2
    putStr ")"
eval (EGe e1 e2) = do
    putStr "("
    eval e1
    putStr " >= "
    eval e2
    putStr ")"
eval (EGt e1 e2) = do
    putStr "("
    eval e1
    putStr " > "
    eval e2
    putStr ")"

eval (EIf eif e1 e2) = do
    putStr "("
    eval eif
    putStr "?"
    eval e1
    putStr ":"
    eval e2
    putStr ")"
    

eval (ELambda (pn, pt) e) = do
    putStr "("
    putStr $ id pn
    putStr "=>("
    eval e
    putStr ")"
    putStr ")"

eval (ELet (s, es) e) = do
    putStr "("
    putStr $ id s
    putStr "=>"
    eval e
    putStr ")("
    eval es
    putStr ")"

eval (ELetRec f (x, tx) (e1, ty) e2) = do
    putStr "var "
    putStr $ id f
    putStr "="
    putStr $ id x
    putStr "=>("
    eval e1
    putStr ");"
    eval e2


eval (EApply e e1) = do
    eval e
    putStr "("
    eval e1
    putStr ")"


translate (Program adts body) = do
    eval body
    putStr "\n"
