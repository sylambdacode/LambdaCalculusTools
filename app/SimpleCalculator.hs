module SimpleCalculator (subcommand) where

import LambdaParser

import qualified Data.Map as Map
import GHC.IO.Handle (hSetEncoding, hGetContents)
import GHC.IO.Encoding (utf8)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle.FD (openFile)
import UntypedLambdaCalculus.LambdaReduction (calculateNormalResult)
import Control.Exception (Exception, throw)
data BaseException = BaseException String

instance Exception BaseException

instance Show BaseException where
    show (BaseException message) = message

subcommand :: String -> String -> IO ()
subcommand functionName codeFile = do
    handle <- openFile codeFile ReadMode
    hSetEncoding handle utf8
    codeContent <- hGetContents handle
    valDefMap <- case parseCode "(code)" codeContent of
        Right result -> return $ valDefListToMap result
        Left e -> throw $ BaseException ("parser error: " ++ show e)
    lambdaTerm <- case Map.lookup functionName valDefMap of
        Just v -> return $ toLambdaTerm valDefMap v
        Nothing -> throw $ BaseException "not found main"

    --print lambdaTerm
    let result = calculateNormalResult lambdaTerm
    print result


