module Control (
                -- *Types
                ControlCommand(..)
               ) where

data ControlCommand = ExitProgram
                    | DoExit
                      deriving Show


