{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module LambdaCalculusTools.LambdaExtension.LambdaExtensionCalculator where

import LambdaCalculusTools.UntypedLambdaCalculus.LambdaTerm (LambdaTerm (Variable, Application), alphaEq)
import LambdaCalculusTools.UntypedLambdaCalculus.LambdaReduction (calculateNormalResult, calculateWeakNormalHeadResult, isWeakNormalHeadForm)
import LambdaCalculusTools.LambdaExtension.LambdaExtensionCalculatorException

import qualified Data.Map as Map
import System.IO (hPutStr, isEOF, stderr)
import System.IO.Error (isEOFError)
import Control.Exception (throw, try)
import Data.Char (chr, ord)
import Data.List (tails, findIndex, isPrefixOf)
import Control.Monad.State (StateT, MonadIO (liftIO))



toSimpleValueString :: String -> LambdaTerm
toSimpleValueString str = Variable ('\'' : str)

fromSimpleValueString :: LambdaTerm -> String
fromSimpleValueString (Variable ('\'' : str)) = str
fromSimpleValueString lambdaTerm = throw (LambdaExtensionCalculatorException ("not string: " ++ show lambdaTerm))

toSimpleValueInt :: Integer -> LambdaTerm
toSimpleValueInt i = Variable ('_' : (show i))

fromSimpleValueInt :: LambdaTerm -> Integer
fromSimpleValueInt (Variable ('_' : i)) = read i
fromSimpleValueInt lambdaTerm = throw (LambdaExtensionCalculatorException ("not int: " ++ show lambdaTerm))

toSimpleValueBool :: Bool -> LambdaTerm
toSimpleValueBool True = Variable "_true"
toSimpleValueBool False = Variable "_false"

fromSimpleValueBool :: LambdaTerm -> Bool
fromSimpleValueBool (Variable "_true") = True
fromSimpleValueBool (Variable "_false") = False
fromSimpleValueBool lambdaTerm = throw (LambdaExtensionCalculatorException ("not bool: " ++ show lambdaTerm))


matchFunction :: (LambdaTerm -> StateT a IO LambdaTerm) -> LambdaTerm -> StateT a IO LambdaTerm

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_strict") arg1) arg2) = do
    arg1Value <- evalLambdaTerm continueEvalLambdaTerm arg1
    return (Application arg2 arg1Value)

matchFunction _ (Application (Application (Variable "_strict-normal") arg1) arg2) = do
    let arg1NormalResult = calculateNormalResult arg1
    return (Application arg2 arg1NormalResult)

matchFunction _ (Application (Variable "_lambda-term-to-string") arg1) = do
    let returnValue = show arg1
    return (toSimpleValueString returnValue)

matchFunction continueEvalLambdaTerm (Application (Variable "_output-string") arg1) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    liftIO $ putStr (fromSimpleValueString arg1val)
    return arg1

matchFunction continueEvalLambdaTerm (Application (Variable "_output-error-string") arg1) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    liftIO $ hPutStr stderr (fromSimpleValueString arg1val)
    return arg1

matchFunction _ (Variable "_input-line") = do
    lineEither <- liftIO $ try getLine
    line <- case lineEither of
        Right v -> return v
        Left e -> if isEOFError (e :: IOError) then return "" else throw e
    return (toSimpleValueString line)

matchFunction _ (Variable "_input-is-eof") = do
    resultVal <- liftIO isEOF
    return (toSimpleValueBool resultVal)

matchFunction continueEvalLambdaTerm (Application (Variable "_int-to-string") arg1) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    let arg1IntegerVal = fromSimpleValueInt arg1val
    return (toSimpleValueString (show arg1IntegerVal))

matchFunction continueEvalLambdaTerm (Application (Variable "_string-to-int") arg1) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    let arg1StringVal = fromSimpleValueString arg1val
    return (toSimpleValueInt (read arg1StringVal))

matchFunction continueEvalLambdaTerm (Application (Variable "_int-to-char") arg1) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    let arg1charval = chr (fromInteger (fromSimpleValueInt arg1val))
    return (toSimpleValueString [arg1charval])

matchFunction continueEvalLambdaTerm (Application (Variable "_char-to-int") arg1) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    let arg1stringval = fromSimpleValueString arg1val
    case arg1stringval of
        c : "" -> return (toSimpleValueInt (toInteger (ord c)))
        "" -> throw (LambdaExtensionCalculatorException "char (string) length must be 1")
        _ -> throw (LambdaExtensionCalculatorException "char (string) length must be 1")

matchFunction continueEvalLambdaTerm (Application (Variable "_string-length") arg1) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    let v = length (fromSimpleValueString arg1val)
    return (toSimpleValueInt (toInteger v))

matchFunction continueEvalLambdaTerm (Application (Application (Application (Variable "_string-substring") arg1) arg2) arg3) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    arg3SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg3
    let arg1val = fromSimpleValueString arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    let arg3val = fromSimpleValueInt arg3SimpleValueValue
    let arg2valInt = fromInteger (arg2val)
    let arg3valInt = fromInteger (arg3val)
    return $ toSimpleValueString (take arg3valInt (drop arg2valInt arg1val))

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_string-concat")  arg1) arg2) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2val <- evalLambdaTerm continueEvalLambdaTerm arg2
    return (toSimpleValueString (fromSimpleValueString arg1val ++ fromSimpleValueString arg2val))

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_string-find") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueString arg1SimpleValueValue
    let arg2val = fromSimpleValueString arg2SimpleValueValue
    resultIndex <- case findIndex (isPrefixOf arg2val) (tails arg1val) of
            Just v -> return v
            Nothing -> return (-1)
    return (toSimpleValueInt (toInteger resultIndex))

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_int-add") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueInt (arg1val + arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_int-sub") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueInt (arg1val - arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_int-mul") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueInt (arg1val * arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_int-div") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueInt (arg1val `div` arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_int-mod") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueInt (arg1val `mod` arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_int-rem") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueInt (arg1val `rem` arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_eq") arg1) arg2) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2val <- evalLambdaTerm continueEvalLambdaTerm arg2
    return $ toSimpleValueBool (alphaEq Map.empty arg1val arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_neq") arg1) arg2) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2val <- evalLambdaTerm continueEvalLambdaTerm arg2
    return $ toSimpleValueBool (not (alphaEq Map.empty arg1val arg2val))

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_int-eq") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueBool (arg1val < arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_int-gt") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueBool (arg1val > arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_int-lt-eq") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueBool (arg1val <= arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Variable "_int-gt-eq") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm continueEvalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueBool (arg1val >= arg2val)

matchFunction continueEvalLambdaTerm (Application (Application (Application (Variable "_if") arg1) arg2) arg3) = do
    arg1val <- evalLambdaTerm continueEvalLambdaTerm arg1
    if fromSimpleValueBool arg1val
        then do
            evalLambdaTerm continueEvalLambdaTerm arg2
        else do
            evalLambdaTerm continueEvalLambdaTerm arg3

matchFunction continueEvalLambdaTerm lambdaTerm = continueEvalLambdaTerm lambdaTerm


evalLambdaTerm :: (LambdaTerm -> StateT a IO LambdaTerm) -> LambdaTerm -> StateT a IO LambdaTerm

evalLambdaTerm continueEvalLambdaTerm lambdaTerm = do
    result <- matchFunction continueEvalLambdaTerm (calculateWeakNormalHeadResult lambdaTerm)
    if isWeakNormalHeadForm result
           then return result
           else evalLambdaTerm continueEvalLambdaTerm result

