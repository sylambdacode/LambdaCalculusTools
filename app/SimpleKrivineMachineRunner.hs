module SimpleKrivineMachineRunner (subcommand) where

import UntypedLambdaCalculus.DeBruijnLambdaTerm(DeBruijnLambdaTerm)
import qualified UntypedLambdaCalculus.DeBruijnLambdaTerm as DeBruijnLambdaTerm
import LambdaParser.UntypedLambdaParser
import UntypedLambdaCalculus.KrivineMachine
import UntypedLambdaCalculus.LambdaTermTools

import CommandException
import CommandArg

import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.TopHandler (flushStdHandles)
import Control.Monad.State (StateT (runStateT), MonadIO (liftIO), MonadState (get, put))
import System.IO (openFile, hSetEncoding, hGetContents, utf8, IOMode(ReadMode), isEOF)
import Control.Exception (throwIO)

get1Or0Char :: IO Char
get1Or0Char = do
    iseof <- isEOF
    if iseof
        then return 'e'
        else do
            c <- getChar
            if c == '0' || c == '1'
            then return c
            else get1Or0Char

transWithIO :: DeBruijnLambdaTerm -> Int -> StateT [Char] IO DeBruijnLambdaTerm
transWithIO wrapper (-1) = do
    liftIO $ putChar '0'
    liftIO flushStdHandles
    return wrapper

transWithIO wrapper (-2) = do
    liftIO $ putChar '1'
    liftIO flushStdHandles
    return wrapper

transWithIO _ n = do
    let inputCount = abs n - 3
    let newInputCount = inputCount + 1
    charList <- get
    inputChar <-
        if length charList <= inputCount
            then do
                c <- liftIO get1Or0Char
                put (c : charList)
                return c
            else do
                let c = charList !! (length charList - inputCount - 1)
                return c
    if inputChar == '1' || inputChar == '0'
    then do
        value <- case inputChar of
            '1' -> return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Variable 2))
            '0' -> return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Variable 1))
            _ -> error "Error"
        return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Application (DeBruijnLambdaTerm.Application (DeBruijnLambdaTerm.Variable 1) value) (DeBruijnLambdaTerm.Variable ((-3) - newInputCount)))
    else return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Variable 1))


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
    let codeFiles = map (\(_, value) -> value) (filter (\(name, _) -> (name == "")) argList)
    valDefList <- parseCodeFiles codeFiles

    let wrapperCode = "ioWrapper___ = λf.f (λx. x O1___ O0___); P___ = ioWrapper___ ((main input___)); "
    wrapperValDefList <- case runParseCode "(wrapperCode)" wrapperCode of
        Right result -> return result
        Left e -> throwIO $ CommandException ("parser error: " ++ show e)

    let valDefMap = valDefListToMap (wrapperValDefList ++ valDefList)

    mainLambdaTerm <- case Map.lookup "P___" valDefMap of
        Just v -> return $ toLambdaTerm Set.empty valDefMap v
        Nothing -> throwIO $ CommandException "not found P___"
    ioWrapper <- case Map.lookup "ioWrapper___" valDefMap of
        Just v -> return $ toLambdaTerm Set.empty valDefMap v
        Nothing -> throwIO $ CommandException "not found ioWrapper___"
    
    let globalFreeVariableMap = Map.fromList [("O0___", -1), ("O1___", -2), ("input___", -3)]
    let ioWrapperDeBruijnLambdaTerm = lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap [Map.empty] ioWrapper
    let deBruijnLambadTerm = lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap [Map.empty] mainLambdaTerm

    let r = krivineMachine (transWithIO ioWrapperDeBruijnLambdaTerm) deBruijnLambadTerm (Environment []) (Environment [])
    _ <- runStateT r []
    return ()


