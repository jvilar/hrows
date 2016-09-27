{-# LANGUAGE Arrows, TupleSections #-}

module Presenter (
                 -- *Functions
                 presenter
                 -- *Reexported
                 , module Input
) where

import Control.Arrow(arr, first, (>>>))
import Control.Auto(Auto, accum_, accumM_, arrM, delay_, fromBlips, emitJusts, holdWith_, perBlip, stepAuto, id, (.))
import Control.Monad(foldM)
import Control.Monad.Writer.Strict(runWriterT)
import Data.Either(partitionEithers)
import Data.Maybe(fromMaybe, isJust)
import Data.Monoid((<>))
import Prelude hiding((.), id)

import GUI.Command
import Input
import Model
import PresenterAuto
import SourceInfo

import ControlAuto
import DialogAuto
import FileAuto
import MovementAuto
import SourceAuto
import UpdateAuto


presenter :: Model -> SourceInfo -> Auto IO Input [GUICommand]
presenter model0 si0 = arr (:[]) >>> updater model0 si0

updater :: Model -> SourceInfo -> Auto IO [Input] [GUICommand]
updater model0 si0 = proc inputs -> do
    rec
        dauto <- delay_ (processInput model0 si0) -< auto
        (cmds, auto) <- arrM (uncurry pr) -< (dauto, inputs)
    id -< cmds

pr :: PresenterAuto Input () -> [Input] -> IO ([GUICommand], PresenterAuto Input ())
pr auto [] = return ([], auto)
pr auto (i:is) = do
    (((), auto'), cmdsIs) <- runWriterT (stepAuto auto i)
    let (cmds, is') = partitionEithers cmdsIs
    (cmds', auto'') <- pr auto' (is ++ is')
    return (cmds ++ cmds', auto'')

processInput :: Model -> SourceInfo -> PresenterAuto Input ()
processInput model0 si0 = proc inp -> do
             rec
               model <- processUpdateCommands model0 -< (inp, pos)
               pos <- processMoveCommands 0 -< (inp, model)
             si <- processSourceCommands si0 -< inp
             processFileCommands -< (inp, model, si)
             processDialogCommands -< inp
             processControlCommands -< inp

processUpdateCommands :: Model -> PresenterAuto (Input, Int) Model
processUpdateCommands model0 = proc (inp, pos) -> do
                bupdates <- emitJusts getUpdates -< inp
                bmodel <- perBlip (updateAuto model0) -< (,pos) <$> bupdates
                model <- holdWith_ model0 -< bmodel
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

processMoveCommands :: Int -> PresenterAuto (Input, Model) Int
processMoveCommands pos0 = proc (inp, model) -> do
                             bmoves <- emitJusts getMoves -< inp
                             bpos <- perBlip (movementAuto pos0) -< (, model) <$> bmoves
                             holdWith_ pos0 -< bpos

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

processSourceCommands :: SourceInfo -> PresenterAuto Input SourceInfo
processSourceCommands si0 = proc inp -> do
                              b <- emitJusts getSources -< inp
                              bsi <- perBlip (sourceAuto si0) -< b
                              holdWith_ si0 -< bsi

getSources :: Input -> Maybe SourceCommand
getSources (InputSource cmd) = Just cmd
getSources _ = Nothing
