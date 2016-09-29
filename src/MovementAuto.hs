module MovementAuto (
                 -- *Types
                movementAuto
) where

import Control.Auto(Auto, accumM_)

import Input
import GUI.Command
import Model
import PresenterAuto

movementAuto :: PresenterAuto (MoveCommand, Model) Int
movementAuto = accumM_ move 0

move :: Int -> (MoveCommand, Model) -> PresenterM Int
move pos (MoveNext, model) = checkedMove (+1) pos model
move pos (MovePrevious, model) = checkedMove (subtract 1) pos model
move pos (MoveHere pos', model) = checkedMove (const $ adjust pos') pos model
                               where adjust p | p < 0 = 0
                                              | p < size model = p
                                              | size model == 0 = 0
                                              | otherwise = size model - 1
move _ (MoveBegin, model) = checkedMove (const 0) 0 model
move _ (MoveEnd, model) = checkedMove (const $ size model - 1) 0 model

checkedMove :: (Int -> Int) -> Int -> Model -> PresenterM Int
checkedMove f pos model | 0 <= pos' && pos' < s = do
                              let r = map toString $ row pos' model
                              sendGUIM $ ShowPosition (pos' + 1) s
                              sendGUIM $ ShowRow r
                              return pos'
                        | pos' == 0 && s == 0 = do
                              sendGUIM $ ShowPosition 0 0
                              sendGUIM DisableTextViews
                              return 0
                        | otherwise = return pos
                        where pos' = f pos
                              s = size model
