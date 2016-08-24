module Input (
              -- *Types
              Input(..)
              -- *Reexported
             , module Movement
) where

import Movement
import AppState

-- |The input that the presenter can receive.
data Input = InputMove MoveCommand

instance StateUpdater Input where
    update (InputMove cmd) = update cmd
