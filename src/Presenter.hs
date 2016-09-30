{-# LANGUAGE Arrows, TupleSections #-}

module Presenter (
                 -- *Functions
                 presenter
                 -- *Reexported
                 , module Presenter.Input
) where

import Control.Arrow(arr, first, (>>>))
import Control.Auto(Auto, accum_, accumM_, arrM, delay_, fromBlips, emitJusts, holdWith_, perBlip, stepAuto, id, (.))
import Control.Monad(foldM)
import Control.Monad.Trans(liftIO)
import Control.Monad.Writer.Strict(runWriterT)
import Data.Either(partitionEithers)
import Data.Maybe(fromMaybe, isJust)
import Data.Monoid((<>))
import Prelude hiding((.), id)

import GUI.Command
import Model
import Presenter.Auto
import Presenter.Input
import SourceInfo

import Presenter.ControlAuto
import Presenter.DialogAuto
import Presenter.FileAuto
import Presenter.MovementAuto
import Presenter.SourceAuto
import Presenter.UpdateAuto

presenter ::  Auto IO Input [GUICommand]
presenter = arr (:[]) >>> updater

updater :: Auto IO [Input] [GUICommand]
updater = proc inputs -> do
    rec
        dauto <- delay_ processInput -< auto
        (cmds, auto) <- arrM (uncurry pr) -< (dauto, inputs)
    arrM putStrLn -< "GUI commands: " ++ show cmds
    id -< cmds

pr :: PresenterAuto Input () -> [Input] -> IO ([GUICommand], PresenterAuto Input ())
pr auto [] = return ([], auto)
pr auto (i:is) = do
    (((), auto'), cmdsIs) <- runWriterT (stepAuto auto i)
    let (cmds, is') = partitionEithers cmdsIs
    (cmds', auto'') <- pr auto' (is ++ is')
    return (cmds ++ cmds', auto'')

processInput :: PresenterAuto Input ()
processInput = proc inp -> do
             arrM (liftIO . putStrLn) -< "inp: " ++ show inp
             rec
               model <- processUpdateCommands -< (inp, pos)
               pos <- processMoveCommands -< (inp, model)
             si <- processSourceCommands -< inp
             processFileCommands -< (inp, model, si)
             processDialogCommands -< inp
             processControlCommands -< inp
             arrM (liftIO . putStrLn) -< "pos': " ++ show pos

processUpdateCommands :: PresenterAuto (Input, Int) Model
processUpdateCommands = proc (inp, pos) -> do
                bupdates <- emitJusts getUpdates -< inp
                bmodel <- perBlip updateAuto -< (,pos) <$> bupdates
                model <- holdWith_ empty -< bmodel
                id -< model

processFileCommands :: PresenterAuto (Input, Model, SourceInfo) ()
processFileCommands = proc (inp, model, si) -> do
                        bfiles <- emitJusts getFileCommands -< inp
                        perBlip fileAuto -< (, model, si) <$> bfiles
                        id -< ()

getUpdates :: Input -> Maybe UpdateCommand
getUpdates (InputUpdate cmd) = Just cmd
getUpdates _ = Nothing

getFileCommands :: Input -> Maybe FileCommand
getFileCommands (InputFile cmd) = Just cmd
getFileCommands _ = Nothing

processMoveCommands :: PresenterAuto (Input, Model) Int
processMoveCommands = proc (inp, model) -> do
                        bmoves <- emitJusts getMoves -< inp
                        bpos <- perBlip movementAuto -< (, model) <$> bmoves
                        holdWith_ 0 -< bpos

getMoves :: Input -> Maybe MoveCommand
getMoves (InputMove cmd) = Just cmd
getMoves _ = Nothing

processDialogCommands :: PresenterAuto Input ()
processDialogCommands = proc inp -> do
                    b <- emitJusts getDialogs -< inp
                    perBlip dialogAuto -< b
                    id -< ()

getDialogs :: Input -> Maybe DialogCommand
getDialogs (InputDialog cmd) = Just cmd
getDialogs _ = Nothing

processControlCommands :: PresenterAuto Input ()
processControlCommands = proc inp -> do
                           b <- emitJusts getControls -< inp
                           perBlip controlAuto -< b
                           id -< ()

getControls :: Input -> Maybe ControlCommand
getControls (InputControl cmd) = Just cmd
getControls _ = Nothing

processSourceCommands :: PresenterAuto Input SourceInfo
processSourceCommands = proc inp -> do
                          b <- emitJusts getSources -< inp
                          bsi <- perBlip (sourceAuto si0) -< b
                          holdWith_ si0 -< bsi
                        where si0 = mkSourceInfo Nothing ()

getSources :: Input -> Maybe SourceCommand
getSources (InputSource cmd) = Just cmd
getSources _ = Nothing
