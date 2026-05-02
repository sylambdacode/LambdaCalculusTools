{-
本文件内容为开发过程中使用的临时代码
-}

-- 用于将直接风格的Lambda演算表达式转化为CPS风格的Lambda演算表达式
-- 本函数的时间复杂度与空间复杂度非常高，本代码暂时仅用于记录转换思路
toCPS :: LambdaTerm -> LambdaTerm -> LambdaTerm
toCPS (LambdaTerm.Variable name) k = LambdaTerm.Application k (LambdaTerm.Variable name)
toCPS (LambdaTerm.Abstraction name lambdaTerm) k =
    LambdaTerm.Application k (LambdaTerm.Abstraction name (LambdaTerm.Abstraction "_variable'" (LambdaTerm.Application (toCPS lambdaTerm k) (LambdaTerm.Variable "_variable"))))
toCPS (LambdaTerm.Application lambdaTerm1 lambdaTerm2) k =
    toCPS lambdaTerm1 (LambdaTerm.Abstraction "_variable1" (toCPS lambdaTerm2 (LambdaTerm.Abstraction "_variable2" (LambdaTerm.Application (LambdaTerm.Application (LambdaTerm.Variable "_variable1") (LambdaTerm.Variable "_variable2")) k))))


