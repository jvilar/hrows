module Presenter.MovementAuto (
                 -- *Types
                movementAuto
) where

import Control.Auto(accumM_)
import Data.List(zipWith4)

import GUI.Command
import Model
import Presenter.Auto
import Presenter.Input

movementAuto :: PresenterAuto (MoveCommand, Model) RowPos
movementAuto = accumM_ move 0

move :: RowPos -> (MoveCommand, Model) -> PresenterM RowPos
move pos (MoveNext, model) = checkedMove (+1) pos model
move pos (MovePrevious, model) = checkedMove (subtract 1) pos model
move pos (MoveHere pos', model) = checkedMove (const $ adjust pos') pos model
                               where adjust p | p < 0 = 0
                                              | p < sm = p
                                              | sm == 0 = 0
                                              | otherwise = sm - 1
                                     sm = size `from` model
move _ (MoveBegin, model) = checkedMove (const 0) 0 model
move _ (MoveEnd, model) = checkedMove (const $ size `from` model - 1) 0 model
move pos (MoveToValue fpos value, model) = checkedMove (const $ nextPos fpos value pos `from` model) pos model

checkedMove :: (RowPos -> RowPos) -> RowPos -> Model -> PresenterM RowPos
checkedMove f pos model | 0 <= pos' && pos' < sm = do
                              let r = zipWith4 combine (row pos' `from` model) (formulas `from` model) (types `from` model) [0..]
                                  combine field mformula fieldType index = FieldInfo { indexFI = index
                                                                                     , textFI = toString field
                                                                                     , formulaFI = mformula
                                                                                     , typeFI = fieldType
                                                                                     , isErrorFI = isError field
                                                                                     , mustWriteFI = True
                                                                                     }
                              sendGUIM $ ShowPosition (pos' + 1) sm
                              sendGUIM $ ShowFields pos' r
                              return pos'
                        | pos' == 0 && sm == 0 = do
                              sendGUIM $ ShowPosition 0 0
                              sendGUIM DisableTextViews
                              return 0
                        | otherwise = return pos
                        where pos' = f pos
                              sm = size `from` model
