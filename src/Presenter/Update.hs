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
                   | DeleteFields [Int]
                   | ChangeFieldType FieldType Int
                     deriving Show
