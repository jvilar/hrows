module Presenter.Input (
              -- *Types
              Input(..)
              -- *Clases
              , IsInput(..)
              -- *Reexported
             , module X
) where

import Presenter.Control as X
import Presenter.Dialog as X
import Presenter.File as X
import Presenter.Movement as X
import Presenter.Update as X
import Presenter.Source as X
import Presenter.Listing as X

-- |The input that the presenter can receive.
data Input = InputMove MoveCommand
           | InputUpdate UpdateCommand
           | InputFile FileCommand
           | InputDialog DialogCommand
           | InputControl ControlCommand
           | InputSource SourceCommand
           | InputListing ListingCommand
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

instance IsInput ListingCommand where
    toInput = InputListing