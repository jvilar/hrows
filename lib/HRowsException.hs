module HRowsException (
    HRowsException(..)
    ) where

import Control.Exception(Exception)
import Data.Typeable(Typeable)
import Data.Text(Text)

data HRowsException = HRowsException Text
                      deriving (Show, Typeable)

instance Exception HRowsException
