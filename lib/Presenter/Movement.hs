module Presenter.Movement (
                 -- *Types
                 MoveCommand(..)
) where

import Data.Text(Text)

import Model.Field

-- |Commands related to movement.
data MoveCommand = MoveNext
                 | MovePrevious
                 | MoveHere Int
                 | MoveBegin
                 | MoveEnd
                 | MoveToValue FieldPos Text deriving Show

