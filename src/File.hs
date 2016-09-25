module File (
              -- *Types
              FileCommand(..)
              -- *Functions
              , fileAuto
              -- *Reexported
              , module SourceInfo
) where

import Control.Auto(Auto, arrM)
import Control.Exception(try)

import GUI.Command
import HRowsException
import Iteration
import ListatabFile
import Model
import SourceInfo

data FileCommand = LoadFile SourceInfo
                 | LoadFileFromName FilePath
                 | WriteFile
                 | WriteFileFromName FilePath
                 deriving Show

fileAuto :: Auto IO (FileCommand, Model) (Maybe Model, [GUICommand])
fileAuto = arrM (uncurry applyCommand)

message :: Message -> [GUICommand]
message = (:[]) . ShowIteration . DisplayMessage

applyCommand :: FileCommand -> Model -> IO (Maybe Model, [GUICommand])
applyCommand (LoadFile (SourceInfo Nothing _)) _ = return (Nothing, message $ ErrorMessage "No puedo cargar un fichero vacío")
applyCommand (LoadFile (SourceInfo (Just fp) (ListatabFormat info))) _ = do
    r <- try $ fromListatab info fp
    putStrLn $ "Read: " ++ fp
    case r of
        Right _ -> putStrLn "OK"
        Left m -> print m
    return $ case r of
        Right model -> (Just model, [])
        Left (HRowsException mess) -> (Nothing, message $ ErrorMessage mess)
applyCommand (LoadFileFromName n) model = do
                                            putStrLn $ "Name: " ++ n
                                            print $ size model
                                            print $ sourceInfo model
                                            print info
                                            applyCommand (LoadFile info) model
        where info = changeFileName n (sourceInfo model)
applyCommand WriteFile model = applyCommand (WriteFileFromName fp) model
        where info = sourceInfo model
              ListatabFormat ltinfo = siFormat info
              Just fp = siFilePath info
applyCommand (WriteFileFromName fp) model = do
        let info = sourceInfo model
            ListatabFormat ltinfo = siFormat info
            model' = if Just fp /= siFilePath info
                     then Just (setSourceInfo (changeFileName fp info) model)
                     else Nothing
        r <- try $ toListatab ltinfo fp model
        case r of
            Right _ -> return (model', message $ InformationMessage "Fichero escrito correctamente.")
            Left (HRowsException m) -> return (Nothing, message $ ErrorMessage m)
