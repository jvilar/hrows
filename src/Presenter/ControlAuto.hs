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

controlAuto :: PresenterAuto (ControlCommand, ModelChanged) ()
controlAuto = arrM processCommand

processCommand :: (ControlCommand, ModelChanged) -> PresenterM ()
processCommand (ExitProgram, c) = sendGUIM $ ShowIteration (ConfirmExit c)
processCommand (DoExit, _) = liftIO exitSuccess

