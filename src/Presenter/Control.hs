module Presenter.Control (
                -- *Types
                ControlCommand(..)
               ) where

data ControlCommand = ExitRequested
                    | ExitProgram
                    | DoExit
                      deriving Show


