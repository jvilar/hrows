module Update (
               -- *Types
               UpdateCommand(..)
) where

import AppState
import Model

data UpdateCommand = UpdateField Int Field deriving Show

instance StateUpdater UpdateCommand where
    update (UpdateField c v) s = return s { model = newModel }
        where newModel = changeField (pos s) c v (model s)
