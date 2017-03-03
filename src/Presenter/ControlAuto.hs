module Presenter.ControlAuto (
               controlAuto
               ) where

import Control.Auto(arrM)
import Control.Monad.Trans(liftIO)
import System.Exit(exitSuccess)

import Model
import GUI.Command
import Presenter.Auto
import Presenter.Control
import Presenter.File

controlAuto :: PresenterAuto (ControlCommand, ModelChanged) ()
controlAuto = arrM processCommand

processCommand :: (ControlCommand, ModelChanged) -> PresenterM ()
processCommand (ExitRequested, c) = sendGUIM $ ShowIteration (ConfirmExit c)
processCommand (ExitProgram, _) = do
                                    sendInputM BackupOnExit
                                    sendInputM DoExit
processCommand (DoExit, _) = liftIO exitSuccess

