module Input (
              -- *Types
              Input(..)
              -- *Reexported
             , module Dialog
             , module File
             , module Movement
             , module Update
) where

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
