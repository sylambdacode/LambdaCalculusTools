module CommandException where

import Control.Exception (Exception)

data CommandException = CommandException String

instance Exception CommandException

instance Show CommandException where
    show (CommandException message) = message


