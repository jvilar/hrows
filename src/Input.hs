module Input (
              -- *Types
              Input(..)
              -- *Clases
              , IsInput(..)
              -- *Reexported
             , module X
) where

import Control as X
import Dialog as X
import File as X
import Movement as X
import Update as X
import Source as X

-- |The input that the presenter can receive.
data Input = InputMove MoveCommand
           | InputUpdate UpdateCommand
           | InputFile FileCommand
           | InputDialog DialogCommand
           | InputControl ControlCommand
           | InputSource SourceCommand
             deriving Show

class IsInput t where
    toInput :: t -> Input

instance IsInput Input where
    toInput = id

instance IsInput MoveCommand where
    toInput = InputMove

instance IsInput UpdateCommand where
    toInput = InputUpdate

instance IsInput FileCommand where
    toInput = InputFile

instance IsInput DialogCommand where
    toInput = InputDialog

instance IsInput ControlCommand where
    toInput = InputControl

instance IsInput SourceCommand where
    toInput = InputSource
