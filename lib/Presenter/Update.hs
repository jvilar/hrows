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
                   | NewFields [(FieldName, FieldType)]
                   | DeleteFields [FieldPos]
                   | RenameFields [FieldName]
                   | ImportFieldsFromRowStore RowStore [(FieldPos, FieldPos)] [(FieldPos, FieldPos)] -- ^Model, keys, fields to replace
                   | ImportRowsFromRowStore RowStore [(FieldPos, FieldPos)] -- ^Model, fields to load from the model
                   | MoveField FieldPos FieldPos
                   | ChangeFieldType FieldType FieldPos
                   | ChangeFieldFormula (Maybe Formula) FieldPos
                   | SetUnchanged
                   | Undo
                   | Redo
                   | BlockUndo
                     deriving Show
