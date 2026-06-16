module UntypedLambdaCalculus.LazyKrivineMachine where

import UntypedLambdaCalculus.DeBruijnLambdaTerm

import Data.Map (Map)
import qualified Data.Map as Map


data HeapElement = Closure (DeBruijnLambdaTerm, [Int]) | Value (DeBruijnLambdaTerm, [Int]) deriving (Show)

data StackElement = Argument Int | Update Int deriving (Show)

krivineMachine :: Monad a => (Int -> a DeBruijnLambdaTerm) -> DeBruijnLambdaTerm -> [StackElement] -> [Int] -> Map Int HeapElement -> a (DeBruijnLambdaTerm, [Int], Map Int HeapElement)

krivineMachine f (Application t u) p e h =
    krivineMachine f t ((Argument ref) : p) e (Map.insert ref (Closure (u, e)) h)
    where ref = Map.size h

krivineMachine f (Abstraction t) ((Argument ref) : p) e h =
    krivineMachine f t p (ref : e) h

krivineMachine f (Abstraction t) ((Update ref) : p) e h =
    krivineMachine f (Abstraction t) p e (Map.insert ref (Value (Abstraction t, e)) h)

krivineMachine f (Variable 1) p (ref : _) h =
    case heapElement of
        Closure (closureLambdaTerm, closureEnvironment) -> krivineMachine f closureLambdaTerm ((Update ref) : p) closureEnvironment h
        Value (valueLambdaTerm, valueClosureEnvironment) -> krivineMachine f valueLambdaTerm p valueClosureEnvironment h
    where heapElement =
              case Map.lookup ref h of
                  Just v -> v
                  Nothing -> error "lookup lazy krivine machine heap error"


krivineMachine f (Variable n) p e h =
    if n > 1
    then krivineMachine f (Variable (n - 1)) p (tail e) h
    else do
        lambdaTerm <- f n
        krivineMachine f lambdaTerm p e h


krivineMachine _ (Abstraction t) [] e h =
    return (Abstraction t, e, h)


