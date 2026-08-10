module UntypedLambdaCalculus.DeBruijnLambdaTerm where

data DeBruijnLambdaTerm = Variable Int | Abstraction DeBruijnLambdaTerm | Application DeBruijnLambdaTerm DeBruijnLambdaTerm


instance Show DeBruijnLambdaTerm where
    show (Variable v) = show v
    show (Abstraction lambdaTerm) = "(λ " ++ show lambdaTerm ++ ")"
    show (Application functionLambdaTerm argumentLambdaTerm) =
        "(" ++ show functionLambdaTerm ++ " " ++ show argumentLambdaTerm ++ ")"
