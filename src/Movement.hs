module Movement (
                 -- *Types
                 MoveCommand(..)
) where

import AppState
import Model

-- |Commands related to movement.
data MoveCommand = MoveNext
                 | MovePrevious
                 | MoveBegin
                 | MoveEnd deriving Show

instance StateUpdater MoveCommand where
    update MoveNext = checkedMovement (+1)
    update MovePrevious = checkedMovement (subtract 1)
    update MoveBegin = checkedMovement (const 0)
    update MoveEnd = \s -> checkedMovement (const $ size (model s) - 1) s

checkedMovement :: (Int -> Int) -> AppState -> AppState
checkedMovement f s | 0 <= newPos && newPos < size (model s) = s { pos = newPos }
                    | otherwise = s
    where newPos = f $ pos s
