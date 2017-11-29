module Presenter.Dialog (
              -- *Types
              DialogCommand(..)
) where

import Model
import Message

data DialogCommand = LoadFileDialog
                   | SaveAsFileDialog
                   | CreateFieldsDialog
                   | DeleteFieldsDialog
                   | ImportFieldsFromDialog
                   | ChooseImportFieldsDialog Model
                   | ChangeNamesDialog
                   | SortRowsDialog
                   | MessageDialog Message
                   | ChangeFieldFormulaDialog Int
                   | SearchFieldDialog Int
                   | CopyOtherDialog Int
                     deriving Show
