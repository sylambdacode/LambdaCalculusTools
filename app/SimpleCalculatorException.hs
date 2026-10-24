{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module SimpleCalculatorException where

import Control.Exception (Exception)


data SimpleCalculatorException = SimpleCalculatorException String

instance Exception SimpleCalculatorException

instance Show SimpleCalculatorException where
    show (SimpleCalculatorException message) = message

