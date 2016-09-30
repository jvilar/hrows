module Presenter.Update (
               -- *Types
               UpdateCommand(..)
) where

import Control.Auto(Auto, accum_)
import Data.Maybe(fromMaybe)

import GUI.Command
import Model

data UpdateCommand = UpdateField Int Field
                   | ChangeModel Model
                   | DoNothing
                   | NewRow
                   | DeleteRow
                   | NewFields [(String, FieldType)]
                     deriving Show
