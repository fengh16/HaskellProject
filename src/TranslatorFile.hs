module TranslatorFile where
    import System.IO
    import AST
    
    eval :: Expr -> Handle -> IO()
    eval (EBoolLit b) outh = 
        case b of
            False -> do
                hPutStr outh "false"
            True -> do
                hPutStr outh "true"
    
    eval (EIntLit b) outh = hPutStr outh  (show b)
    eval (ECharLit b) outh = hPutStr outh (show b)
    eval (EVar b) outh= hPutStr outh $ id b
    
    eval (ENot e) outh= do
        hPutStr outh "!"
        eval e outh
    eval (EAnd e1 e2) outh= do
        eval e1 outh
        hPutStr outh " && "
        eval e2 outh
    
    eval (EOr e1 e2) outh= do
        eval e1 outh
        hPutStr outh " || "
        eval e2 outh
    eval (EAdd e1 e2) outh= do
        eval e1 outh
        hPutStr outh " + "
        eval e2 outh
    
    eval (ESub e1 e2) outh= do
        hPutStr outh "("
        eval e1 outh
        hPutStr outh " - "
        eval e2 outh
        hPutStr outh ")"
    eval (EMul e1 e2) outh= do
        eval e1 outh
        hPutStr outh " * "
        eval e2 outh
    eval (EDiv e1 e2) outh= do
        hPutStr outh "Math.floor("
        eval e1 outh
        hPutStr outh " / "
        eval e2 outh
        hPutStr outh ")"
    eval (EMod e1 e2) outh= do
        hPutStr outh "("
        eval e1 outh
        hPutStr outh " % "
        eval e2 outh
        hPutStr outh ")"
    
    eval (EEq e1 e2) outh= do
        eval e1 outh
        hPutStr outh " == "
        eval e2 outh
    
    eval (ENeq e1 e2) outh= do
        eval e1 outh
        hPutStr outh " != "
        eval e2 outh
    
    eval (ELe e1 e2) outh= do
        eval e1 outh
        hPutStr outh " <= "
        eval e2 outh
    eval (ELt e1 e2) outh= do
        eval e1 outh
        hPutStr outh " < "
        eval e2 outh
    eval (EGe e1 e2) outh= do
        eval e1 outh
        hPutStr outh " >= "
        eval e2 outh
    eval (EGt e1 e2) outh= do
        eval e1 outh
        hPutStr outh " > "
        eval e2 outh
    
    eval (EIf eif e1 e2) outh= do
        hPutStr outh "("
        eval eif outh
        hPutStr outh "?"
        eval e1 outh
        hPutStr outh ":"
        eval e2 outh
        hPutStr outh ")"
        
    eval (ELambda (pn, pt) e) outh= do
        hPutStr outh "function("
        hPutStr outh $ id pn
        hPutStr outh "){\n\treturn "
        eval e outh
        hPutStr outh "}"
    
    eval (ELet (s, es) e) outh= do
        hPutStr outh "(function("
        hPutStr outh $ id s
        hPutStr outh "){\n\treturn "
        eval e outh
        hPutStr outh "\n})("
        eval es outh
        hPutStr outh ")\n"
    
    eval (ELetRec f (x, tx) (e1, ty) e2) outh= do
        hPutStr outh "const "
        hPutStr outh $ id f
        hPutStr outh "= function("
        hPutStr outh $ id x
        hPutStr outh "){\n\treturn "
        eval e1 outh
        hPutStr outh "\n};\n"
        eval e2 outh
    
    
    eval (EApply e e1) outh= do
        eval e outh
        hPutStr outh "("
        eval e1 outh
        hPutStr outh ")"
    
    
    translate (Program adts body) = do
        outh <- openFile "output.js" WriteMode
        eval body outh
        hPutStr outh "\n"
        hClose outh
    