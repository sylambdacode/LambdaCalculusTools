{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module LambdaCalculusTools.CommandLineInterface.CommandException where

import Control.Exception (Exception)

data CommandException = CommandException String

instance Exception CommandException

instance Show CommandException where
    show (CommandException message) = message


