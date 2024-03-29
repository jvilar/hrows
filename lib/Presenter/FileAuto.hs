{-# LANGUAGE OverloadedStrings #-}

module Presenter.FileAuto (
              -- *Functions
              fileAuto
) where

import Control.Auto(arrM)
import Control.Exception(SomeException(..), try)
import Control.Monad(void, when)
import Control.Monad.Trans(liftIO)
import qualified Data.Text as T
import System.Directory(removeFile)

import GUI.Command
import HRowsException
import Model
import Model.DefaultFileNames
import Model.RowStore.RowStoreConf
import Presenter.Input
import Presenter.Auto

fileAuto :: PresenterAuto (FileCommand, Model, SourceInfo) ()
fileAuto = arrM (uncurry3 applyCommand)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

message :: Message -> PresenterM ()
message = sendGUIM . ShowIteration . DisplayMessage

applyCommand :: FileCommand -> Model -> SourceInfo -> PresenterM ()
applyCommand LoadFile _ si = do
    r <- liftIO $ try $ readRowStore si
    case r of
        Right (rst, mconf) -> do
            let model = case mconf of
                  Nothing -> fromRowStore rst
                  Just cnf -> foldr addSourceInfo (fromRowStore rst) $ sourceInfos cnf
            sendInputM $ ChangeModel model
            sendInputM $ SetMainSource si
        Left (HRowsException mess) -> message $ ErrorMessage mess

applyCommand (LoadFileFromName pc) model info = do
    let info' = changePathAndConf pc info
    applyCommand LoadFile model info'

applyCommand WriteFile model si = doWrite model si False

applyCommand (WriteFileFromName pc) model si = let
    si' = changePathAndConf pc si
  in doWrite model si' True

applyCommand (ImportFromFile t si) _ _ = do
    r <- liftIO $ try $ readRowStore si
    case r of
        Right (rst, _) -> sendInputM $ ChooseImportDialog t rst
        Left (HRowsException m) -> message $ ErrorMessage m

applyCommand (AddSourceFromSourceInfo name si) _ _  = do
    r <- liftIO $ try $ readRowStore si
    case r of
        Right (rst, _) -> sendInputM . AddNewSource si $ setName name rst
        Left (HRowsException m) -> message $ ErrorMessage m

applyCommand WriteBackup model si = do
    when (changed `from` model) $ do
        let conf = defaultBackupFileName <$> confPath (siPathAndConf si)
            fp = defaultBackupFileName $ path (siPathAndConf si)
            si' = changePathAndConf (PathAndConf fp conf) si
        r <- liftIO $ try $ writeRowStore si' (getSourceInfos model) <@ model
        case r of
            Right _ -> return ()
            Left (HRowsException m) -> message $ ErrorMessage ("Error al hacer la copia de seguridad: " `T.append` m)

applyCommand BackupOnExit model si
    | changed `from` model = applyCommand WriteBackup model si
    | otherwise = do
                    let conf = defaultBackupFileName <$> confPath (siPathAndConf si)
                        fp = defaultBackupFileName $ path (siPathAndConf si)
                    void . liftIO $ ((try $ do
                              removeFile fp
                              maybe (return ()) removeFile conf) :: IO (Either SomeException ()))

doWrite :: Model -> SourceInfo -> Bool -> PresenterM ()
doWrite model si changedSource = do
        r <- liftIO $ try $ writeRowStore si (getSourceInfos model) <@ model
        case r of
            Right _ -> do
                          message $ InformationMessage "Fichero escrito correctamente."
                          when changedSource $ sendInputM $ SetMainSource si
                          sendInputM SetUnchanged
            Left (HRowsException m) -> message $ ErrorMessage m

