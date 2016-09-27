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

fileAuto :: PresenterAuto (FileCommand, Model, SourceInfo) ()
fileAuto = arrM (uncurry3 applyCommand)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

message :: Message -> PresenterM ()
message = sendGUIM . ShowIteration . DisplayMessage

applyCommand :: FileCommand -> Model -> SourceInfo -> PresenterM ()
applyCommand LoadFile _ (SourceInfo Nothing _) = message $ ErrorMessage "No puedo cargar un fichero vacío"
applyCommand LoadFile _ info@(SourceInfo (Just fp) (ListatabFormat format)) = do
    r <- liftIO $ do
        r <- try $ fromListatab format fp
        putStrLn $ "Read: " ++ fp
        case r of
            Right _ -> putStrLn "OK"
            Left m -> print m
        return r
    case r of
        Right model -> do
            sendInputM $ ChangeModel model
            sendInputM $ SetSource info
        Left (HRowsException mess) -> message $ ErrorMessage mess
applyCommand (LoadFileFromName n) model info = do
    let info' = changeFileName n info
    liftIO $ do
        putStrLn $ "Name: " ++ n
        print $ size model
        print $ info'
    applyCommand LoadFile model info'
applyCommand WriteFile model info = applyCommand (WriteFileFromName fp) model info
        where ListatabFormat ltinfo = siFormat info
              Just fp = siFilePath info
applyCommand (WriteFileFromName fp) model info = do
        let ListatabFormat ltinfo = siFormat info
        r <- liftIO $ try $ toListatab ltinfo fp model
        case r of
            Right _ -> do
                          message $ InformationMessage "Fichero escrito correctamente."
                          when (Just fp /= siFilePath info) $
                              sendInputM $ SetSource (changeFileName fp info)
            Left (HRowsException m) -> message $ ErrorMessage m
