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
                   | ImportFieldsFromModel Model [(FieldPos, FieldPos)] [(FieldPos, FieldPos)] -- ^Model, keys, fields to replace
                   | ImportRowsFromModel Model [(FieldPos, FieldPos)] -- ^Model, fields to load from the model
                   | MoveField FieldPos FieldPos
                   | ChangeFieldType FieldType FieldPos
                   | ChangeFieldFormula (Maybe Formula) FieldPos
                   | SetUnchanged
                   | Undo
                   | Redo
                   | BlockUndo
                     deriving Show
