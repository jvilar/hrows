module Presenter.Dialog (
              -- *Types
              DialogCommand(..)
              -- *Reexported
              , ImportType(..)
) where

import Model
import Message
import Presenter.ImportType

data DialogCommand = LoadFileDialog
                   | SaveAsFileDialog
                   | CreateFieldsDialog
                   | DeleteFieldsDialog
                   | ImportFromDialog ImportType
                   | ChooseImportDialog ImportType RowStore
                   | ChangeNamesDialog
                   | SortRowsDialog
                   | MessageDialog Message
                   | ChangeFieldFormulaDialog FieldPos
                   | SearchFieldDialog FieldPos
                   | CopyOtherDialog FieldPos
                   | AddSourceDialog
                   | DeleteSourceDialog
                     deriving Show
