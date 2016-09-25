module Input (
              -- *Types
              Input(..)
              -- *Reexported
             , module X
) where

import Dialog as X
import File as X
import Movement as X
import Update as X

-- |The input that the presenter can receive.
data Input = InputMove MoveCommand
           | InputUpdate UpdateCommand
           | InputFile FileCommand
           | InputDialog DialogCommand
             deriving Show
