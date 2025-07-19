module KrivineMachine where

import DeBruijnLambdaTerm

newtype Environment = Environment [(DeBruijnLambdaTerm, Environment)] deriving Show

type Stack = Environment

krivineMachine :: Monad a => (Int -> a DeBruijnLambdaTerm) -> DeBruijnLambdaTerm -> Stack -> Environment -> a (DeBruijnLambdaTerm, Environment)

krivineMachine f (Application t u) (Environment p) e =
    krivineMachine f t (Environment ((u, e) : p)) e

krivineMachine f (Abstraction t) (Environment ((u, e') : p)) (Environment e) =
    krivineMachine f t (Environment p) (Environment ((u, e') : e))

krivineMachine f (Variable 1) p (Environment ((t, e'):e)) =
    krivineMachine f t p e'

krivineMachine f (Variable n) p (Environment e) =
    if n >= 1
    then krivineMachine f (Variable (n - 1)) p (Environment (tail e))
    else do
        lambdaTerm <- f n
        krivineMachine f lambdaTerm p (Environment e)

krivineMachine f (Abstraction t) (Environment []) e =
    return (Abstraction t, e)

krivineMachine _ _ _ _ = error "Error"