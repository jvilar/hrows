module HRowsException (
    exception,
    HRowsException(..),
    hRowsException
    ) where

import Control.Exception(displayException, Exception, IOException, throwIO)
import Data.Typeable(Typeable)
import Data.Text(pack, Text)

data HRowsException = HRowsException Text
                      deriving (Show, Typeable)

instance Exception HRowsException

-- |Auxiliary function to help creating the exceptions from a `String`.
hRowsException :: String -> HRowsException
hRowsException = HRowsException . pack

-- |Auxiliary function to throw a normal `IOException` as an `HRowException`. 
exception :: IOException -> IO a
exception e = throwIO $ hRowsException $ "Exception: " ++ displayException e

