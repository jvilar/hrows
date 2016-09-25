module Control (
                -- *Types
                ControlCommand(..)
               , controlAuto
               ) where

import Control.Auto(Auto, arrM)
import System.Exit(exitSuccess)

import GUI.Command

data ControlCommand = ExitProgram
                    | DoExit
                      deriving Show

controlAuto :: Auto IO ControlCommand [GUICommand]
controlAuto = arrM processCommand

processCommand :: ControlCommand -> IO [GUICommand]
processCommand ExitProgram = return [ShowIteration ConfirmExit]
processCommand DoExit = exitSuccess

