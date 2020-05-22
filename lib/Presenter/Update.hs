module Presenter.Update (
               -- *Types
               UpdateCommand(..)
               -- *Rexports
               , FieldPos
               , SortDirection(..)
) where

import Data.Text(Text)

import Model
import Model.SourceInfo (SourceInfo)

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
                   | AddNewSource SourceInfo RowStore 
                   | Undo
                   | Redo
                   | BlockUndo
                     deriving Show
