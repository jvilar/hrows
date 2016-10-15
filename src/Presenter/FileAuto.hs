module Presenter.FileAuto (
              -- *Functions
              fileAuto
) where

import Control.Auto(Auto, arrM)
import Control.Exception(try)
import Control.Monad(when)
import Control.Monad.Trans(liftIO)
import Data.Maybe(isJust)

import GUI.Command
import HRowsException
import Model
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
applyCommand (LoadFileFromName n loadConf) model info = do
    let info' = changeFileName n $
                changeConfFileName (
                    if loadConf
                    then Just (defaultConfFileName n)
                    else Nothing
                  ) info
    applyCommand LoadFile model info'
applyCommand WriteFile model info = applyCommand (WriteFileFromName fp (isJust $ siConfFile info)) model info
        where ListatabFormat ltinfo = siFormat info
              Just fp = siFilePath info
applyCommand (WriteFileFromName fp saveConf) model info = do
        let ListatabFormat ltinfo = siFormat info
            conf = if saveConf
                   then Just $ (defaultConfFileName fp)
                   else Nothing
        r <- liftIO $ try $ toListatab ltinfo fp conf model
        case r of
            Right _ -> do
                          message $ InformationMessage "Fichero escrito correctamente."
                          when (Just fp /= siFilePath info) $
                              sendInputM $ SetSource (changeFileName fp info)
            Left (HRowsException m) -> message $ ErrorMessage m
