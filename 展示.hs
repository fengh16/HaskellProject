-- 展示说明：
-- 在ghci中，按顺序copy以下代码并执行。
-- 【请在Notepad++中打开该txt，确保copy时缩进换行正确性】

-- 1. 定义 Maybe Int 和 [Int]:
maybeInt = ADT "MaybeInt" [("JustInt",[TInt]),("Nothing",[])]
arrayInt = ADT "ArrayInt" [("Nil",[]),("Cons",[TInt ,(TData "ArrayInt")])]



-- 2. 定义 map | filter | (==)
-- 2.1 map
mapbody = ELetRec "map" ("xs",TData "ArrayInt") ((ECase (EVar "xs") [(PData "Nil" [],(EVar "Nil")),((PData "Cons" [PVar "x",PVar "xs1"]),EApply (EApply (EVar "Cons") (EApply (EVar "f") (EVar "x"))) (EApply (EVar "map") (EVar "xs1")))]),TData "ArrayInt") (EApply (EVar "map") (EVar "xss"))
map = ELambda  ("f",(TArrow TInt TInt )) (ELambda ("xss",TData "ArrayInt") mapbody)

-- 2.2 filter
:{
filterExpr = (ECase (EVar "xs") 
                [
                    (
                        PData "Nil" [],
                        (EVar "Nil")
                    ),
                    (
                        (PData "Cons" [PVar "x",PVar "xs1"]),
                        EIf 
                            (EApply (EVar "f") (EVar "x")) 
                            (EApply (EApply (EVar "Cons") (EVar "x")) (EApply (EVar "filter") (EVar "xs1")))
                            (EApply (EVar "filter") (EVar "xs1"))
                    )
                ])
:}
filterbody = ELetRec "filter" ("xs",TData "ArrayInt") (filterExpr,TData "ArrayInt") (EApply (EVar "filter") (EVar "xss"))
filter = ELambda  ("f",(TArrow TInt TBool )) (ELambda ("xss",TData "ArrayInt") filterbody)

-- 2.3 (==)
:{
equalExpr = ELambda ("ys",TData "ArrayInt") (ECase (EVar "xs") 
                [
                    (
                        (PData "Nil" []),
                        (ECase (EVar "ys") 
                        [
                            (
                                (PData "Nil" []),
                                (EBoolLit True)
                            ),
                            (
                                (PData "Cons" [PVar "y",PVar "ys1"]),
                                (EBoolLit False)
                            )
                        ])
                    ),
                    (
                        (PData "Cons" [PVar "x",PVar "xs1"]),
                        (ECase (EVar "ys")
                        [
                            (
                                (PData "Nil" []),
                                (EBoolLit False)
                            ),
                            (
                                (PData "Cons" [PVar "y",PVar "ys1"]),
                                EIf 
                                    (EEq (EVar "x") (EVar "y"))
                                    (EApply (EApply (EVar "equal") (EVar "xs1")) (EVar "ys1"))
                                    (EBoolLit False)
                            )
                        ])
                    )
                ])
:}
equalbody = ELetRec "equal" ("xs",TData "ArrayInt") (equalExpr,(TArrow (TData "ArrayInt") TBool)) ((EApply (EApply (EVar "equal") (EVar "xss")) (EVar "yss")))
equal = ELambda ("xss",TData "ArrayInt") (ELambda ("yss",TData "ArrayInt") (equalbody))



-- 3. 定义 saveDiv
safeDiv = ELambda ("x",TInt) (ELambda ("y",TInt) (EIf (EEq (EVar "y") (EIntLit 0)) (EVar "Nothing") (EApply (EVar "JustInt") (EDiv (EVar "x") (EVar "y")))))



-- 4. 运行如下计算程序
-- 4.1 safeDiv 7 2
evalProgram (Program [arrayInt,maybeInt] (EApply (EApply safeDiv (EIntLit 7)) (EIntLit 2)))

-- 4.2 safeDiv 7 0
evalProgram (Program [arrayInt,maybeInt] (EApply (EApply safeDiv (EIntLit 7)) (EIntLit 0)))

-- 4.3 [1,2] == [1,2]
list1 = (EApply (EApply (EVar "Cons") (EIntLit 1)) ((EApply (EApply (EVar "Cons") (EIntLit 2)) (EVar "Nil"))))
evalProgram (Program [arrayInt,maybeInt] (EApply (EApply equal list1) list1))

-- 4.4 [1,2] == [1]
list2 = ((EApply (EApply (EVar "Cons") (EIntLit 1)) (EVar "Nil")))
evalProgram (Program [arrayInt,maybeInt] (EApply (EApply equal list1) list2))

-- 4.5 map (\x -> x * x) [1,2,3]
list3 = (EApply (EApply (EVar "Cons") (EIntLit 1)) (EApply (EApply (EVar "Cons") (EIntLit 2)) ((EApply (EApply (EVar "Cons") (EIntLit 3)) (EVar "Nil")))))
square = ELambda ("x",TInt) (EMul (EVar "x") (EVar "x"))
evalProgram (Program [arrayInt,maybeInt] (EApply (EApply map square) list3))

-- 4.6 filter even [1,2,3]
even = ELambda ("x",TInt) (EEq (EMod (EVar "x") (EIntLit 2)) (EIntLit 0))
evalProgram (Program [arrayInt,maybeInt] (EApply (EApply filter even) list3))