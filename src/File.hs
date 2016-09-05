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
import Model
import SourceInfo

data FileCommand = LoadFile SourceInfo
                 | LoadFileFromName FilePath
                 deriving Show

instance StateUpdater FileCommand where
    update (LoadFile EmptySource) s = return s { pendingIteration = DisplayMessage $ ErrorMessage "No puedo cargar un fichero vacío" }
    update (LoadFile (ListatabSource info)) s = do
        r <- try $ fromListatab info
        case r of
            Right m -> return s { model = m, pendingIteration = NoIteration }
            Left (HRowsException message) -> return s { pendingIteration = DisplayMessage $ ErrorMessage message }
    update (LoadFileFromName n) s = update (LoadFile info) s
        where info = changeFileName n (sourceInfo $ model s)
