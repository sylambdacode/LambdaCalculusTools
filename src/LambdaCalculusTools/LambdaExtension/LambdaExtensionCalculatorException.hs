{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module LambdaCalculusTools.LambdaExtension.LambdaExtensionCalculatorException where

import Control.Exception (Exception)


data LambdaExtensionCalculatorException = LambdaExtensionCalculatorException String

instance Exception LambdaExtensionCalculatorException

instance Show LambdaExtensionCalculatorException where
    show (LambdaExtensionCalculatorException message) = message

