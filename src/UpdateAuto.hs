module UpdateAuto (
    updateAuto
) where

import Control.Arrow(first)
import Control.Auto(Auto, accumM_)
import Data.Maybe(fromMaybe)

import Input
import GUI.Command
import Model
import PresenterAuto

updateAuto :: PresenterAuto (UpdateCommand, Int) Model
updateAuto = accumM_ update empty

update :: Model -> (UpdateCommand, Int) -> PresenterM Model
update model (UpdateField c v, pos) = do
    let f = row pos model' !! c
        model' = changeField pos c v model
    sendGUIM $ ShowFieldState c $ if isError f
                                  then ErrorFieldState
                                  else NormalFieldState
    return model'
update _ (ChangeModel model, _) = do
    sendGUIM $ ShowNames (cnames model)
    sendInputM MoveBegin
    return model
update model (DoNothing, _) = return model
update model (NewRow, _) = do
    sendInputM MoveEnd
    return $ addEmptyRow model
update model (DeleteRow, pos) = do
    sendInputM $ MoveHere pos
    return $ deleteRow pos model
update model (NewFields l, pos) = do
    sendInputM $ MoveHere pos
    let model' = newFields (map (first Just) l) model
    sendGUIM $ ShowNames (cnames model')
    return model'


cnames :: Model -> [String]
cnames = map (++ ": ") . fnames
