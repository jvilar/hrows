{-# LANGUAGE OverloadedStrings #-}

module Presenter.FileAuto (
              -- *Functions
              fileAuto
) where

import Control.Auto(arrM)
import Control.Exception(SomeException(..), try)
import Control.Monad(void, when)
import Control.Monad.Trans(liftIO)
import Data.Default(def)
import Data.Maybe(fromJust, isJust)
import qualified Data.Text as T
import System.Directory(removeFile)

import GUI.Command
import HRowsException
import Model
import Model.DefaultFileNames
import Model.RowStore
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
applyCommand LoadFile _ si = do
    r <- liftIO $ try $ readRowStore si
    case r of
        Right rs -> do
            sendInputM $ ChangeModel $ fromRowStore rs
            sendInputM $ SetMainSource si
        Left (HRowsException mess) -> message $ ErrorMessage mess

applyCommand (LoadFileFromName n conf) model info = do
    let info' = changeFileName n $
                changeConfFileName conf info
    applyCommand LoadFile model info'

applyCommand WriteFile model si = doWrite model si False

applyCommand (WriteFileFromName fp conf) model si = let
    si' = changeConfFileName conf $ changeFileName fp si
  in doWrite model si' True

applyCommand (ImportFromFile t si) _ _ = do
    r <- liftIO $ try $ readRowStore si
    case r of
        Right rst -> sendInputM $ ChooseImportDialog t rst
        Left (HRowsException m) -> message $ ErrorMessage m

applyCommand (AddSourceFromSourceInfo name si) _ _  = do
    r <- liftIO $ try $ readRowStore si
    case r of
        Right rst -> sendInputM . AddNewSource si $ setName name rst
        Left (HRowsException m) -> message $ ErrorMessage m

applyCommand WriteBackup model si = do
    let fp = defaultBackupFileName <$> siFilePath si
    when (isJust fp && changed `from` model) $ do
        let conf = defaultBackupFileName <$> siConfFile si
            si' = changeConfFileName conf $ changeFileName (fromJust fp) si
        r <- liftIO $ try $ writeRowStore si' (getSourceInfos model) <@ model
        case r of
            Right _ -> return ()
            Left (HRowsException m) -> message $ ErrorMessage ("Error al hacer la copia de seguridad: " `T.append` m)

applyCommand BackupOnExit model si
    | changed `from` model = applyCommand WriteBackup model si
    | otherwise = do
                    let fp = defaultBackupFileName <$> siFilePath si
                        conf = defaultBackupFileName <$> siConfFile si
                    void . liftIO $ ((try $ do
                              maybe (return ()) removeFile fp
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
        
