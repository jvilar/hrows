module ControlAuto (
               controlAuto
               ) where

import Control.Auto(Auto, arrM)
import Control.Monad.Trans(liftIO)
import System.Exit(exitSuccess)

import Control
import GUI.Command
import PresenterAuto

controlAuto :: PresenterAuto ControlCommand ()
controlAuto = arrM processCommand

processCommand :: ControlCommand -> PresenterM ()
processCommand ExitProgram = sendGUIM $ ShowIteration ConfirmExit
processCommand DoExit = liftIO exitSuccess

