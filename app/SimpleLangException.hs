module SimpleLangException where

import Control.Exception (Exception)


data SimpleLangException = SimpleLangException String

instance Exception SimpleLangException

instance Show SimpleLangException where
    show (SimpleLangException message) = message

