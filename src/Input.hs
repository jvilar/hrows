module Input (
              -- *Types
              Input(..)
              -- *Reexported
             , module Dialog
             , module File
             , module Movement
             , module Update
) where

import AppState

import Dialog
import File
import Movement
import Update

-- |The input that the presenter can receive.
data Input = InputMove MoveCommand
           | InputUpdate UpdateCommand
           | InputFile FileCommand
           | InputDialog DialogCommand
             deriving Show

instance StateUpdater Input where
    update (InputMove _) = undefined
    update (InputUpdate cmd) = undefined
    update (InputFile cmd) = update cmd
    update (InputDialog cmd) = update cmd

