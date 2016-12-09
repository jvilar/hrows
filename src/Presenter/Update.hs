module Presenter.Update (
               -- *Types
               UpdateCommand(..)
) where

import Model

data UpdateCommand = UpdateField FieldPos Field
                   | ChangeModel Model
                   | DoNothing
                   | NewRow
                   | DeleteRow
                   | NewFields [(String, FieldType)]
                   | DeleteFields [FieldPos]
                   | RenameFields [String]
                   | MoveField FieldPos FieldPos
                   | ChangeFieldType FieldType FieldPos
                   | ChangeFieldFormula (Maybe Formula) FieldPos
                     deriving Show
