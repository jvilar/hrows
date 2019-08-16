{-# LANGUAGE DeriveGeneric #-}

module GUI.Control (
            -- *Types
            GUIControl
            , GUIControl'(..)
            -- *Functions
            , sendInput
) where

import Control.Concurrent.Chan(Chan, writeChan)
import Data.IORef(IORef)
import Data.BitVector(BitVector)
import GHC.Generics(Generic)
import GI.Gtk

import Presenter.Input

import GUI.Command
import GUI.HKD
import GUI.MainWindow

data GUIControl' f = GUIControl { mainWindow :: HKD f MainWindow
                                , inputChan :: HKD f (Chan Input)
                                , changeFieldFormulaDialog :: HKD f Dialog
                                , changeFieldFormulaEntry :: HKD f Entry
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

type GUIControl = GUIControl' Identity

sendInput :: IsInput cmd => GUIControl -> cmd -> IO ()
sendInput control = writeChan (inputChan control) . toInput
