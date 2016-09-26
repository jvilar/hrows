module FileAuto (
              -- *Functions
              fileAuto
) where

import Control.Auto(Auto, arrM)
import Control.Exception(try)
import Control.Monad(when)
import Control.Monad.Trans(liftIO)

import GUI.Command
import HRowsException
import Input
import ListatabFile
import Model
import PresenterAuto
import SourceInfo

fileAuto :: PresenterAuto (FileCommand, Model) ()
fileAuto = arrM (uncurry applyCommand)

message :: Message -> PresenterM ()
message = sendGUIM . ShowIteration . DisplayMessage

applyCommand :: FileCommand -> Model -> PresenterM ()
applyCommand (LoadFile (SourceInfo Nothing _)) _ = message $ ErrorMessage "No puedo cargar un fichero vacío"
applyCommand (LoadFile (SourceInfo (Just fp) (ListatabFormat info))) _ = do
    r <- liftIO $ do
        r <- try $ fromListatab info fp
        putStrLn $ "Read: " ++ fp
        case r of
            Right _ -> putStrLn "OK"
            Left m -> print m
        return r
    case r of
        Right model -> sendInputM $ ChangeModel model
        Left (HRowsException mess) -> message $ ErrorMessage mess
applyCommand (LoadFileFromName n) model = do
    let info = changeFileName n (sourceInfo model)
    liftIO $ do
        putStrLn $ "Name: " ++ n
        print $ size model
        print $ sourceInfo model
        print info
    applyCommand (LoadFile info) model
applyCommand WriteFile model = applyCommand (WriteFileFromName fp) model
        where info = sourceInfo model
              ListatabFormat ltinfo = siFormat info
              Just fp = siFilePath info
applyCommand (WriteFileFromName fp) model = do
        let info = sourceInfo model
            ListatabFormat ltinfo = siFormat info
        when (Just fp /= siFilePath info) $
            sendInputM $ ChangeModel (setSourceInfo (changeFileName fp info) model)
        r <- liftIO $ try $ toListatab ltinfo fp model
        case r of
            Right _ -> message $ InformationMessage "Fichero escrito correctamente."
            Left (HRowsException m) -> message $ ErrorMessage m
