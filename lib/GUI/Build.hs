{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module GUI.Build (
            -- *Types
            GUIControl
            -- *Functions
            , makeGUI
) where

import Control.Concurrent.Chan(Chan, writeChan)
import Control.Monad(forM_, void, when)
import Data.BitVector(nil)
import Data.Functor.Identity(Identity)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Maybe(isJust, fromJust)
import Data.Text(Text)
import qualified Data.Text as T

import GI.Gdk (screenGetDefault, EventKey, keyvalName, ModifierType(..))
import GI.Gtk hiding (MessageDialog)

import Paths_hrows(getDataFileName)

import GUI.CanBeCast
import GUI.Control
import GUI.BuildMonad
import GUI.Iteration
import GUI.MainWindow
import GUI.MainWindow.Build
import GUI.HKD
import Model.Field
import Presenter.ImportType
import Presenter.Input

makeGUI :: Chan Input -> IO GUIControl
makeGUI iChan = do
  GI.Gtk.init Nothing

  builder <- builderNew
  gladefn <- getDataFileName "src/hrows.glade"
  builderAddFromFile builder $ T.pack gladefn

  styleFile <- getDataFileName "src/hrows.css"

  provider <- cssProviderNew
  cssProviderLoadFromPath provider $ T.pack styleFile
  Just screen <- screenGetDefault
  styleContextAddProviderForScreen screen provider $ fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION

  control <- prepareControl iChan builder
  runBuild  builder control buildMainWindow
  return control


prepareControl :: Chan Input -> Builder -> IO GUIControl
prepareControl iChan builder = do
  let getObject :: CanBeCast obj => Text -> IO obj
      getObject name = builderGetObject builder name >>= doCast . fromJust
  fromIO GUIControl {
    mainWindow = getMainWindow iChan builder
    , inputChan = return iChan
    , changeFieldFormulaDialog = getObject "changeFieldFormulaDialog"
    , changeFieldFormulaEntry = getObject "changeFieldFormulaEntry"
    , changeFieldFormulaLabel = getObject "changeFieldFormulaLabel"
    , changeFieldFormulaButton = getObject "changeFieldFormulaCheckButton"
    , confFileSaveCheckButton =  getObject "confFileSaveCheckButton"
    , saveAsDialog = getObject "saveAsDialog"
    , confFileLoadCheckButton = getObject "confFileLoadCheckButton"
    , loadFileDialog = getObject "loadFileDialog"
    , importFromFileDialog = getObject "importFromFileDialog"
    , importInputSeparator = getObject "importInputSeparator"
    , importFieldsOptionsDialog = getObject "importFieldsOptionsDialog"
    , importFieldsOptionsRows = getObject "importFieldsOptionsRows"
    , importRowsOptionsDialog = getObject "importRowsOptionsDialog"
    , importRowsOptionsRows = getObject "importRowsOptionsRows"
    , searchFieldDialog = getObject "searchFieldDialog"
    , searchFieldCombo = getObject "searchFieldCombo"
    , copyOtherDialog = getObject "copyOtherDialog"
    , copyOtherCombo = getObject "copyOtherCombo"
    }



notImplementedDialog :: Text -> Input
notImplementedDialog f = toInput $ MessageDialog (InformationMessage  $ T.concat ["Opción ", f, " no implementada"])
