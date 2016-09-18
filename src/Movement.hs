module Movement (
                 -- *Types
                 MoveCommand(..)
                , movementAuto
) where

import Control.Auto(Auto, accum_)

import GUICommand
import Model

-- |Commands related to movement.
data MoveCommand = MoveNext
                 | MovePrevious
                 | MoveBegin
                 | MoveEnd deriving Show

movementAuto :: Int -> Auto IO (MoveCommand, Model) (Int, [GUICommand])
movementAuto pos0 = accum_ (move . fst) (pos0, [])

move :: Int -> (MoveCommand, Model) -> (Int, [GUICommand])
move pos (MoveNext, model) = checkedMove (+1) pos model
move pos (MovePrevious, model) = checkedMove (subtract 1) pos model
move pos (MoveBegin, model) = checkedMove (const 0) pos model
move pos (MoveEnd, model) = checkedMove (const $ size model - 1) pos model

checkedMove :: (Int -> Int) -> Int -> Model -> (Int, [GUICommand])
checkedMove f pos model | 0 <= pos' && pos' < s = (pos', [posCmd, rowCmd])
                        | otherwise = (pos, [])
                        where pos' = f pos
                              s = size model
                              posCmd = ShowPosition (pos' + 1) s
                              rowCmd = ShowRow r
                              r = map toString $ row pos' model
