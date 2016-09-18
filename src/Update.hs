module Update (
               -- *Types
               UpdateCommand(..)
              , updateAuto
) where

import Control.Auto(Auto, accum_)

import Model

data UpdateCommand = UpdateField Int Field deriving Show

updateAuto :: Model -> Auto IO (UpdateCommand, Int) Model
updateAuto = accum_ update

update :: Model -> (UpdateCommand, Int) -> Model
update model (UpdateField c v, pos) = changeField pos c v model
