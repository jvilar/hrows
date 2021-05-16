{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module GUI.DialogManager.Build (
  buildDialogManager
  , configureDialogManager
) where

import Data.Maybe(fromJust)
import Data.Text(Text)
import qualified Data.Text as T

import GI.Gtk hiding (MessageDialog)

import GUI.BuildMonad
import GUI.CanBeCast
import GUI.DialogManager
import GUI.Iteration
import GUI.HKD
import Presenter.Input

buildDialogManager :: Builder -> IO DialogManager
buildDialogManager builder = do
  let getObject :: CanBeCast obj => Text -> IO obj
      getObject name = builderGetObject builder name >>= doCast . fromJust
  fromIO DialogManager {
    changeFieldFormulaDialog = getObject "changeFieldFormulaDialog"
    , changeFieldFormulaEntry = getObject "changeFieldFormulaEntry"
    , changeFieldFormulaLabel = getObject "changeFieldFormulaLabel"
    , changeFieldFormulaButton = getObject "changeFieldFormulaCheckButton"
    , confFileSaveCheckButton =  getObject "confFileSaveCheckButton"
    , saveAsDialog = getObject "saveAsDialog"
    , confFileLoadCheckButton = getObject "confFileLoadCheckButton"
    , loadFileDialog = getObject "loadFileDialog"
    , importFromFileDialog = getObject "importFromFileDialog"
    , importInputSeparator = getObject "importInputSeparator"
    , importInputFormat = getObject "importInputFormat"
    , importFieldsOptionsDialog = getObject "importFieldsOptionsDialog"
    , importFieldsOptionsRows = getObject "importFieldsOptionsRows"
    , importRowsOptionsDialog = getObject "importRowsOptionsDialog"
    , importRowsOptionsRows = getObject "importRowsOptionsRows"
    , searchFieldDialog = getObject "searchFieldDialog"
    , searchFieldCombo = getObject "searchFieldCombo"
    , copyOtherDialog = getObject "copyOtherDialog"
    , copyOtherCombo = getObject "copyOtherCombo"
    , showSourcesDialog = getObject "showSourcesDialog"
    , sourcesTreeView = getObject "sourcesTreeView"
    , aboutDialog = getObject "aboutDialog"
    }

configureDialogManager :: BuildMonad()
configureDialogManager = do
  prepareChangeFieldFormulaDialog

prepareChangeFieldFormulaDialog :: BuildMonad ()
prepareChangeFieldFormulaDialog = do
    dmg <- getDialogManager
    let btn = changeFieldFormulaButton dmg
        entry = changeFieldFormulaEntry dmg
    ioVoid $ btn `on` #toggled $ toggleButtonGetActive btn >>=
                                 widgetSetSensitive entry


notImplementedDialog :: Text -> Input
notImplementedDialog f = toInput $ MessageDialog (InformationMessage  $ T.concat ["Opción ", f, " no implementada"])
