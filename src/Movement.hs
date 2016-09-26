module Movement (
                 -- *Types
                 MoveCommand(..)
) where

-- |Commands related to movement.
data MoveCommand = MoveNext
                 | MovePrevious
                 | MoveBegin
                 | MoveEnd deriving Show

