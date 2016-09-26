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

import ControlAuto
import DialogAuto
import FileAuto
import MovementAuto
import UpdateAuto

presenter :: Model -> Auto IO Input [GUICommand]
presenter model0 = arr (:[]) >>> updater model0

updater :: Model -> Auto IO [Input] [GUICommand]
updater model0 = proc inputs -> do
    rec
        dauto <- delay_ (processInput model0) -< auto
        (cmds, auto) <- arrM (uncurry pr) -< (dauto, inputs)
    id -< cmds

pr :: PresenterAuto Input () -> [Input] -> IO ([GUICommand], PresenterAuto Input ())
pr auto [] = return ([], auto)
pr auto (i:is) = do
    (((), auto'), cmdsIs) <- runWriterT (stepAuto auto i)
    let (cmds, is') = partitionEithers cmdsIs
    (cmds', auto'') <- pr auto' (is ++ is')
    return (cmds ++ cmds', auto'')

processInput :: Model -> PresenterAuto Input ()
processInput model0 = proc inp -> do
             rec
               model <- processUpdateCommands model0 -< (inp, pos)
               pos <- processMoveCommands 0 -< (inp, model)
             processFileCommands -< (inp, model)
             processDialogCommands -< inp
             processControlCommands -< inp

processUpdateCommands :: Model -> PresenterAuto (Input, Int) Model
processUpdateCommands model0 = proc (inp, pos) -> do
                bupdates <- emitJusts getUpdates -< inp
                bmodel <- perBlip (updateAuto model0) -< (,pos) <$> bupdates
                model <- holdWith_ model0 -< bmodel
                id -< model

processFileCommands :: PresenterAuto (Input, Model) ()
processFileCommands = proc (inp, model) -> do
                        bfiles <- emitJusts getFileCommands -< inp
                        perBlip fileAuto -< (, model) <$> bfiles
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
