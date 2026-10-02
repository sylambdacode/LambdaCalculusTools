{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module UntypedLambdaCalculus.KrivineMachine where

import UntypedLambdaCalculus.DeBruijnLambdaTerm

newtype Environment = Environment [(DeBruijnLambdaTerm, Environment)] deriving Show

type Stack = Environment

krivineMachine :: Monad a => (Int -> a DeBruijnLambdaTerm) -> DeBruijnLambdaTerm -> Stack -> Environment -> a (DeBruijnLambdaTerm, Environment)

krivineMachine f (Application t u) (Environment p) e =
    krivineMachine f t (Environment ((u, e) : p)) e

krivineMachine f (Abstraction t) (Environment ((u, e') : p)) (Environment e) =
    krivineMachine f t (Environment p) (Environment ((u, e') : e))

krivineMachine f (Variable 1) p (Environment ((t, e'):_)) =
    krivineMachine f t p e'

krivineMachine f (Variable n) p (Environment (_ : e)) =
    if n >= 1
    then krivineMachine f (Variable (n - 1)) p (Environment e)
    else do
        lambdaTerm <- f n
        krivineMachine f lambdaTerm p (Environment e)

krivineMachine _ (Variable _) _ (Environment []) = error "Environment not be empty"

krivineMachine _ (Abstraction t) (Environment []) e =
    return (Abstraction t, e)
