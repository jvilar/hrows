module Input (
              -- *Types
              Input(..)
              -- *Reexported
             , module File
             , module Movement
             , module Update
) where

import AppState
import File
import Movement
import Update

-- |The input that the presenter can receive.
data Input = InputMove MoveCommand
           | InputUpdate UpdateCommand
           | InputFile FileCommand
             deriving Show

instance StateUpdater Input where
    update (InputMove cmd) = update cmd
    update (InputUpdate cmd) = update cmd
    update (InputFile cmd) = update cmd
