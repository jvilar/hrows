module HRowsException (
    HRowsException(..)
    ) where

import Control.Exception(Exception)
import Data.Typeable(Typeable)

data HRowsException = HRowsException String
                      deriving (Show, Typeable)

instance Exception HRowsException
