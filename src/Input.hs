module Input (
              -- *Types
              Input(..)
              -- *Reexported
             , module Movement
             , module Update
) where

import AppState
import Movement
import Update

-- |The input that the presenter can receive.
data Input = InputMove MoveCommand
           | InputUpdate UpdateCommand

instance StateUpdater Input where
    update (InputMove cmd) = update cmd
    update (InputUpdate cmd) = update cmd
