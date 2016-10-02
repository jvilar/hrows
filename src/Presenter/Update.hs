module Presenter.Update (
               -- *Types
               UpdateCommand(..)
) where

import Model

data UpdateCommand = UpdateField Int Field
                   | ChangeModel Model
                   | DoNothing
                   | NewRow
                   | DeleteRow
                   | NewFields [(String, FieldType)]
                   | DeleteField Int
                   | ChangeFieldType FieldType Int
                     deriving Show
