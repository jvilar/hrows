module Presenter.FileAuto (
              -- *Functions
              fileAuto
) where

import Control.Auto(Auto, arrM)
import Control.Exception(SomeException(..), try)
import Control.Monad(unless, void, when)
import Control.Monad.Trans(liftIO)
import Data.Maybe(fromJust, isJust)
import System.Directory(removeFile)

import GUI.Command
import HRowsException
import Model
import Model.DefaultFileNames
import Model.ListatabFile
import Model.SourceInfo
import Presenter.Input
import Presenter.Auto

fileAuto :: PresenterAuto (FileCommand, Model, SourceInfo) ()
fileAuto = arrM (uncurry3 applyCommand)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

message :: Message -> PresenterM ()
message = sendGUIM . ShowIteration . DisplayMessage

applyCommand :: FileCommand -> Model -> SourceInfo -> PresenterM ()
applyCommand LoadFile _ (SourceInfo Nothing _ _) = message $ ErrorMessage "No puedo cargar un fichero vacío"
applyCommand LoadFile _ info@(SourceInfo (Just fp) conffp (ListatabFormat format)) = do
    r <- liftIO $ try $ fromListatab format fp conffp
    case r of
        Right model -> do
            sendInputM $ ChangeModel model
            sendInputM $ SetSource info
        Left (HRowsException mess) -> message $ ErrorMessage mess
applyCommand (LoadFileFromName n conf) model info = do
    let info' = changeFileName n $
                changeConfFileName conf info
    applyCommand LoadFile model info'
applyCommand WriteFile model info = applyCommand (WriteFileFromName fp (siConfFile info)) model info
        where ListatabFormat ltinfo = siFormat info
              Just fp = siFilePath info
applyCommand (WriteFileFromName fp conf) model info = do
        let ListatabFormat ltinfo = siFormat info
        r <- liftIO $ try $ toListatab ltinfo fp conf model
        case r of
            Right _ -> do
                          message $ InformationMessage "Fichero escrito correctamente."
                          sendInputM $ ChangeModel (setUnchanged model)
                          when (Just fp /= siFilePath info) $
                              sendInputM $ SetSource (changeFileName fp info)
            Left (HRowsException m) -> message $ ErrorMessage m
applyCommand WriteBackup model info = do
    let ListatabFormat ltinfo = siFormat info
        fp = defaultBackupFileName <$> siFilePath info
        conf = defaultBackupFileName <$> siConfFile info
    when (isJust fp && changed model) $ do
        r <- liftIO $ try $ toListatab ltinfo (fromJust fp) conf model
        case r of
            Right _ -> return ()
            Left (HRowsException m) -> message $ ErrorMessage ("Error al hacer la copia de seguridad: " ++ m)
applyCommand RemoveBackup model info = do
    let fp = defaultBackupFileName <$> siFilePath info
        conf = defaultBackupFileName <$> siConfFile info
    unless (changed model) $ do
        r <- liftIO $ try $ do
                 maybe (return ()) removeFile fp
                 maybe (return ()) removeFile conf
        return $ case r of
                     Right _ -> ()
                     Left (SomeException _) -> ()
