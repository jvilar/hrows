module Presenter.Movement (
                 -- *Types
                 MoveCommand(..)
) where

-- |Commands related to movement.
data MoveCommand = MoveNext
                 | MovePrevious
                 | MoveHere Int
                 | MoveBegin
                 | MoveEnd
                 | MoveToValue Int String deriving Show

