module SimpleLang (subcommand) where

import UntypedLambdaCalculus.LambdaTerm
import LambdaParser.UntypedLambdaParser
import UntypedLambdaCalculus.LambdaReduction (calculateWeakNormalHeadResult)
import CommandException
import CommandArg
import SimpleLangException

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO (openFile, hSetEncoding, hGetContents, utf8, IOMode(ReadMode))
import System.IO.Error (isEOFError)
import Control.Exception (throw, throwIO, try)
import Data.Char (chr, ord)
import Control.Monad.State (StateT (runStateT), MonadIO (liftIO), MonadState (get, put))

data ObjectState = ObjectState Integer (Map String (Map String String))

toSimpleLangString :: String -> String
toSimpleLangString str = ('\'' : str)

fromSimpleLangString :: String -> String
fromSimpleLangString ('\'' : str) = str
fromSimpleLangString _ = throw (SimpleLangException "not string")

toSimpleLangInt :: Integer -> String
toSimpleLangInt i = show i

fromSimpleLangInt :: String -> Integer
fromSimpleLangInt i = read i

toSimpleLangBool :: Bool -> String
toSimpleLangBool True = "true"
toSimpleLangBool False = "false"

fromSimpleLangBool :: String -> Bool
fromSimpleLangBool "true" = True
fromSimpleLangBool "false" = False
fromSimpleLangBool _ = throw (SimpleLangException "not bool")

toSimpleLangObjectIndex :: Integer -> String
toSimpleLangObjectIndex index = '@' : (show index)


matchFunction :: LambdaTerm -> StateT ObjectState IO String

matchFunction (Application (Application (Variable "string-concat")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    return (toSimpleLangString (fromSimpleLangString arg1val ++ fromSimpleLangString arg2val))

matchFunction (Application (Application (Variable "print")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    liftIO $ putStr (fromSimpleLangString arg1val)
    evalExpr (Application arg2 (Variable "'"))

matchFunction (Application (Variable "readline")  arg1) = do
    lineEither <- liftIO (try getLine)
    (line, isEof) <- case lineEither of
        Right line -> return (line, False)
        Left e -> if isEOFError (e :: IOError) then return ("", True) else throw e
    evalExpr (Application (Application arg1 (Variable (toSimpleLangString line))) (Variable (toSimpleLangBool isEof)))

matchFunction (Application (Application (Variable "strict")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    evalExpr (Application arg2 (Variable arg1val))

matchFunction (Application (Variable "int-to-string")  arg1) = do
    arg1val <- evalExpr arg1
    return (toSimpleLangString arg1val)

matchFunction (Variable "map-create") = do
    (ObjectState mapCount objectMap) <- get
    let simpleLangIntMapCount = toSimpleLangObjectIndex mapCount
    put (ObjectState (mapCount + 1) (Map.insert simpleLangIntMapCount Map.empty objectMap))
    return simpleLangIntMapCount

matchFunction (Application (Application (Application (Variable "map-put") arg1) arg2) arg3) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    arg3val <- evalExpr arg3
    (ObjectState mapCount objectMap) <- get
    let simpleLangIntMapCount = toSimpleLangObjectIndex mapCount
    arg1Map <- case Map.lookup arg1val objectMap of
        Just v -> return v
        Nothing -> throw (SimpleLangException "no map")
    let resultMap = Map.insert arg2val arg3val arg1Map
    put (ObjectState (mapCount + 1) (Map.insert simpleLangIntMapCount resultMap objectMap))
    return simpleLangIntMapCount

matchFunction (Application (Application (Variable "map-delete") arg1) arg2)= do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    (ObjectState mapCount objectMap) <- get
    let simpleLangIntMapCount = toSimpleLangObjectIndex mapCount
    arg1Map <- case Map.lookup arg1val objectMap of
        Just v -> return v
        Nothing -> throw (SimpleLangException "no map")
    let resultMap = Map.delete arg2val arg1Map
    put (ObjectState (mapCount + 1) (Map.insert simpleLangIntMapCount resultMap objectMap))
    return simpleLangIntMapCount

matchFunction (Application (Application (Variable "map-has-key") arg1) arg2) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    (ObjectState _ objectMap) <- get
    arg1Map <- case Map.lookup arg1val objectMap of
        Just v -> return v
        Nothing -> throw (SimpleLangException "no map")
    return (toSimpleLangBool (Map.member arg2val arg1Map))

matchFunction (Application (Application (Variable "map-get") arg1) arg2) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    (ObjectState _ objectMap) <- get
    arg1Map <- case Map.lookup arg1val objectMap of
        Just v -> return v
        Nothing -> throw (SimpleLangException "no map")
    result <- case Map.lookup arg2val arg1Map of
        Just v -> return v
        Nothing -> throw (SimpleLangException "no map key")
    return result

matchFunction (Application (Variable "map-size") arg1) = do
    arg1val <- evalExpr arg1
    (ObjectState _ objectMap) <- get
    arg1Map <- case Map.lookup arg1val objectMap of
        Just v -> return v
        Nothing -> throw (SimpleLangException "no map")
    let result = Map.size arg1Map
    return (toSimpleLangInt (toInteger result))

matchFunction (Application (Application (Application (Variable "map-fold")  arg1) arg2) arg3) = do
    arg1val <- evalExpr arg1
    arg3val <- evalExpr arg3
    (ObjectState _ objectMap) <- get
    arg1Map <- case Map.lookup arg1val objectMap of
        Just v -> return v
        Nothing -> throw (SimpleLangException "no map")
    result <- Map.foldlWithKey foldFunc (return arg3val) arg1Map
    return result
    where foldFunc v key value = do
              v' <- v
              r <- evalExpr (Application (Application (Application arg2 (Variable v')) (Variable key)) (Variable value))
              return r

matchFunction (Application (Variable "map-destroy")  arg1) = do
    arg1val <- evalExpr arg1
    (ObjectState objectCount objectMap) <- get
    let objectMap' = Map.delete arg1val objectMap
    put (ObjectState objectCount objectMap')
    return arg1val

matchFunction (Application (Variable "string-to-int")  arg1) = do
    arg1val <- evalExpr arg1
    return (fromSimpleLangString arg1val)

matchFunction (Application (Variable "int-to-char")  arg1) = do
    arg1val <- evalExpr arg1
    let arg1charval = chr (fromInteger (fromSimpleLangInt arg1val))
    return (toSimpleLangString [arg1charval])

matchFunction (Application (Variable "char-to-int")  arg1) = do
    arg1val <- evalExpr arg1
    let arg1stringval = fromSimpleLangString arg1val
    case arg1stringval of
        c : "" -> return (toSimpleLangInt (toInteger (ord c)))
        "" -> throw (SimpleLangException "char (string) length must be 1")
        _ -> throw (SimpleLangException "char (string) length must be 1")

matchFunction (Application (Variable "string-length")  arg1) = do
    arg1val <- evalExpr arg1
    let v = length (fromSimpleLangString arg1val)
    return (toSimpleLangInt (toInteger v))

matchFunction (Application (Application (Application (Variable "string-substring")  arg1) arg2) arg3) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    arg3SimpleLangValue <- evalExpr arg3
    let arg1val = fromSimpleLangString arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    let arg3val = fromSimpleLangInt arg3SimpleLangValue
    let arg2valInt = fromInteger (arg2val)
    let arg3valInt = fromInteger (arg3val)
    return $ toSimpleLangString (take arg3valInt (drop arg2valInt arg1val))

matchFunction (Application (Application (Variable "int-add")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangInt (arg1val + arg2val)

matchFunction (Application (Application (Variable "int-sub")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangInt (arg1val - arg2val)

matchFunction (Application (Application (Variable "int-mul")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangInt (arg1val * arg2val)

matchFunction (Application (Application (Variable "int-div")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangInt (arg1val `div` arg2val)

matchFunction (Application (Application (Variable "eq")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    return $ toSimpleLangBool (arg1val == arg2val)

matchFunction (Application (Application (Variable "neq")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    return $ toSimpleLangBool (arg1val /= arg2val)

matchFunction (Application (Application (Variable "int-lt")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangBool (arg1val < arg2val)

matchFunction (Application (Application (Variable "int-gt")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangBool (arg1val > arg2val)

matchFunction (Application (Application (Variable "int-lteq")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangBool (arg1val <= arg2val)

matchFunction (Application (Application (Variable "int-gteq")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangBool (arg1val >= arg2val)

matchFunction (Application (Application (Application (Variable "if")  arg1) arg2) arg3) = do
    arg1val <- evalExpr arg1
    if fromSimpleLangBool arg1val
        then do
            evalExpr arg2
        else do
            evalExpr arg3


matchFunction (Variable a) = return a
matchFunction lambdaTerm = throw (SimpleLangException ("match function error: " ++ show lambdaTerm))


evalExpr :: LambdaTerm -> StateT ObjectState IO String
evalExpr lambdaTerm = do
    let l = calculateWeakNormalHeadResult lambdaTerm
    v <- matchFunction l
    return v

parseCodeFiles :: [String] -> IO ([ValDef])
parseCodeFiles [] = return []
parseCodeFiles (codeFile : codeFiles) = do
    handle <- openFile codeFile ReadMode
    hSetEncoding handle utf8
    codeContent <- hGetContents handle
    valDefList <- case runParseCode codeFile codeContent of
        Right result -> return result
        Left e -> throwIO $ CommandException ("parser error: " ++ show e)
    valDefList' <- parseCodeFiles codeFiles
    return (valDefList ++ valDefList')


subcommand :: [String] -> IO ()
subcommand args = do
    let argList = parseArgs args
    functionName <- case lookup "f" argList of
        Just v -> return v
        Nothing -> return "main"
    let codeFiles = map (\(_, value) -> value) (filter (\(name, _) -> (name == "")) argList)
    valDefList <- parseCodeFiles codeFiles
    let valDefMap = valDefListToMap valDefList
    lambdaTerm <- case Map.lookup functionName valDefMap of
        Just v -> return $ toLambdaTerm Set.empty valDefMap v
        Nothing -> throwIO $ CommandException ("not found " ++ functionName)
    result <- try (runStateT (evalExpr (readLambdaTerm (show lambdaTerm))) (ObjectState 0 Map.empty))
    case result of
        Left e -> throwIO (CommandException ("SimpleLang runtime error: " ++ show (e :: SimpleLangException)))
        Right _ -> return ()
