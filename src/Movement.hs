module Movement (
                 -- *Types
                 MoveCommand(..)
                , movementAuto
) where

import Control.Auto(Auto, accum)

import Model

-- |Commands related to movement.
data MoveCommand = MoveNext
                 | MovePrevious
                 | MoveBegin
                 | MoveEnd deriving Show

movementAuto :: Int -> Auto IO (MoveCommand, Model) Int
movementAuto = accum move

move :: Int -> (MoveCommand, Model) -> Int
move pos (MoveNext, model) | pos < size model - 1 = pos + 1
                           | otherwise = pos
move pos (MovePrevious, _) | pos > 0 = pos - 1
                           | otherwise = pos
move _ (MoveBegin, _) = 0
move _ (MoveEnd, m) = size m - 1
