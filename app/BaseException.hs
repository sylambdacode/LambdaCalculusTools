module BaseException where

import Control.Exception (Exception)

data BaseException = BaseException String

instance Exception BaseException

instance Show BaseException where
    show (BaseException message) = message


