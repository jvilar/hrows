module UpdateAuto (
    updateAuto
) where

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
    let r = map toString $ row pos model'
        model' = changeField pos c v model
    sendGUIM $ ShowRow r
    return model'
update _ (ChangeModel model, _) = do
    sendGUIM $ ShowNames (fnames model)
    sendInputM MoveBegin
    return model

fnames :: Model -> [String]
fnames model = map (++ ": ") $ fromMaybe
                   (map (("Campo " ++).show) [1 .. ncols model])
                   (names model)