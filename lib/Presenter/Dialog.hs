module Presenter.Dialog (
              -- *Types
              DialogCommand(..)
) where

import Model
import Message
import Presenter.ImportType

data DialogCommand = LoadFileDialog
                   | SaveAsFileDialog
                   | CreateFieldsDialog
                   | DeleteFieldsDialog
                   | ImportFromDialog ImportType
                   | ChooseImportDialog ImportType Model
                   | ChangeNamesDialog
                   | SortRowsDialog
                   | MessageDialog Message
                   | ChangeFieldFormulaDialog FieldPos
                   | SearchFieldDialog FieldPos
                   | CopyOtherDialog FieldPos
                     deriving Show
