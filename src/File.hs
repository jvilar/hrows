module File (
              -- *Types
              FileCommand(..)
            -- *Reexported
              , module SourceInfo
) where

import Control.Exception(try)

import AppState
import HRowsException
import ListatabFile
import Message
import SourceInfo

data FileCommand = LoadFile SourceInfo
                 deriving Show

instance StateUpdater FileCommand where
    update (LoadFile EmptySource) s = return s { pendingMessage = Just $ ErrorMessage "No puedo cargar un fichero vacío" }
    update (LoadFile (ListatabSource info)) s = do
        r <- try $ fromListatab info
        case r of
            Right m -> return s { model = m }
            Left (HRowsException message) -> return s { pendingMessage = Just $ ErrorMessage message }
