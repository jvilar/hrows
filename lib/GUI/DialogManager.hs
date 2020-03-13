{-# LANGUAGE DeriveGeneric #-}
module GUI.DialogManager (
  -- *Types
  DialogManager
  , DialogManager'(..)
  ) where

import GHC.Generics(Generic)
import GI.Gtk

import GUI.HKD

data DialogManager' f = DialogManager { changeFieldFormulaDialog :: HKD f Dialog
                                      , changeFieldFormulaEntry :: HKD f TextView
                                      , changeFieldFormulaLabel :: HKD f Label
                                      , changeFieldFormulaButton :: HKD f CheckButton
                                      , confFileSaveCheckButton :: HKD f CheckButton
                                      , saveAsDialog :: HKD f FileChooserDialog
                                      , confFileLoadCheckButton :: HKD f CheckButton
                                      , loadFileDialog :: HKD f FileChooserDialog
                                      , importFromFileDialog :: HKD f FileChooserDialog
                                      , importInputSeparator :: HKD f Entry
                                      , importFieldsOptionsDialog :: HKD f Dialog
                                      , importFieldsOptionsRows :: HKD f Grid
                                      , importRowsOptionsDialog :: HKD f Dialog
                                      , importRowsOptionsRows :: HKD f Grid
                                      , searchFieldDialog :: HKD f Dialog
                                      , searchFieldCombo :: HKD f ComboBoxText
                                      , copyOtherDialog :: HKD f Dialog
                                      , copyOtherCombo :: HKD f ComboBoxText
                                      } deriving Generic

type DialogManager = DialogManager' Identity
