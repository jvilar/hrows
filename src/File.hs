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
                 | WriteFile
                 | WriteFileFromName FilePath
                 deriving Show

instance StateUpdater FileCommand where
    update (LoadFile EmptySource) s = addMessageM (ErrorMessage "No puedo cargar un fichero vacío") s
    update (LoadFile (ListatabSource info)) s = do
        r <- try $ fromListatab info
        case r of
            Right m -> cancelIterationM s >>= update m
            Left (HRowsException message) -> update (ErrorMessage message) s
    update (LoadFileFromName n) s = update (LoadFile info) s
        where info = changeFileName n (sourceInfo $ model s)
    update WriteFile s = do
        let ListatabSource info = sourceInfo $ model s
            m = model s
        r <- try $ toListatab info m
        case r of
            Right _ -> update m s >>= update (InformationMessage "Fichero escrito correctamente.")
            Left (HRowsException message) -> update (ErrorMessage message) s
    update (WriteFileFromName n) s = do
        let ListatabSource info = changeFileName n (sourceInfo $ model s)
            m = setSourceInfo info $ model s
        r <- try $ toListatab info m
        case r of
            Right _ -> update m s >>= update (InformationMessage "Fichero escrito correctamente.")
            Left (HRowsException message) -> update (ErrorMessage message) s
