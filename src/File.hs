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
    update (LoadFile (SourceInfo Nothing _)) s = addMessageM (ErrorMessage "No puedo cargar un fichero vacío") s
    update (LoadFile (SourceInfo (Just fp) (ListatabFormat info))) s = do
        r <- try $ fromListatab info fp
        case r of
            Right m -> cancelIterationM s >>= update m
            Left (HRowsException message) -> update (ErrorMessage message) s
    update (LoadFileFromName n) s = update (LoadFile info) s
        where info = changeFileName n (sourceInfo $ model s)
    update WriteFile s = update (WriteFileFromName fp) s
        where info = sourceInfo $ model s
              ListatabFormat ltinfo = siFormat info
              Just fp = siFilePath info
    update (WriteFileFromName fp) s = do
        let info = sourceInfo $ model s
            ListatabFormat ltinfo = siFormat info
            m = model s
        r <- try $ toListatab ltinfo fp m
        case r of
            Right _ -> update m s >>= update (InformationMessage "Fichero escrito correctamente.")
            Left (HRowsException message) -> update (ErrorMessage message) s
