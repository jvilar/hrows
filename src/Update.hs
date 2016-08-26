module Update (
               -- *Types
               UpdateCommand(..)
) where

import AppState
import Model

data UpdateCommand = UpdateField Int Field

instance StateUpdater UpdateCommand where
    update (UpdateField c v) s = s { model = newModel }
        where newModel = changeField (pos s) c v (model s)
