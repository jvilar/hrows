module File (
              -- *Types
              FileCommand(..)
            -- *Reexported
              , module SourceInfo
) where

import AppState
import ListatabFile
import SourceInfo

data FileCommand = LoadFile SourceInfo
                 deriving Show

instance StateUpdater FileCommand where
    update (LoadFile EmptySource) s = return s { errorMessage = Just "No puedo cargar un fichero vacío" }
    update (LoadFile (ListatabSource info)) s = do
                                    m <- fromListatab info
                                    return s { model = m }
