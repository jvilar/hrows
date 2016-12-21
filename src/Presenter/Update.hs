module Presenter.Update (
               -- *Types
               UpdateCommand(..)
               -- *Rexportso
               , FieldPos
               , SortDirection(..)
) where

import Model

data UpdateCommand = UpdateField FieldPos Field
                   | ChangeModel Model
                   | DoNothing
                   | NewRow
                   | DeleteRow
                   | SortRows FieldPos SortDirection
                   | NewFields [(String, FieldType)]
                   | DeleteFields [FieldPos]
                   | RenameFields [String]
                   | MoveField FieldPos FieldPos
                   | ChangeFieldType FieldType FieldPos
                   | ChangeFieldFormula (Maybe Formula) FieldPos
                     deriving Show
