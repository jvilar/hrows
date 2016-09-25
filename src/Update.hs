module Update (
               -- *Types
               UpdateCommand(..)
              , updateAuto
) where

import Control.Auto(Auto, accum_)
import Data.Maybe(fromMaybe)

import GUICommand
import Model

data UpdateCommand = UpdateField Int Field
                   | ChangeModel Model
                     deriving Show

updateAuto :: Model -> Auto IO (UpdateCommand, Int) (Model, [GUICommand])
updateAuto model0 = accum_ (update . fst) (model0, [])

update :: Model -> (UpdateCommand, Int) -> (Model, [GUICommand])
update model (UpdateField c v, pos) = (model', [ShowRow r])
                                      where r = map toString $ row pos model'
                                            model' = changeField pos c v model
update _ (ChangeModel model, _) = (model, [ShowNames (fnames model)])

fnames :: Model -> [String]
fnames model = map (++ ": ") $ fromMaybe
                   (map (("Campo " ++).show) [1 .. ncols model])
                   (names model)
