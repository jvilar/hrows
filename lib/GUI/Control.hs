{-# LANGUAGE OverloadedStrings #-}

module GUI.Control (
            -- *Types
            GUIControl(..)
            -- *Functions
            , sendInput
) where

import Control.Concurrent.Chan(Chan, writeChan)
import Data.IORef(IORef)
import Graphics.UI.Gtk

import Presenter.Input

data GUIControl = GUIControl { mainWindow :: Window
                             , positionLabel :: Label
                             , fieldsGrid :: Grid
                             , numberOfFields :: IORef Int
                             , currentField :: IORef Int
                             , inputChan :: Chan Input
                             , fieldMenu :: Menu
                             , beginButton :: Button
                             , endButton :: Button
                             , leftButton :: Button
                             , rightButton :: Button
                             , changeFieldFormulaDialog :: Dialog
                             , changeFieldFormulaEntry :: Entry
                             , changeFieldFormulaLabel :: Label
                             , changeFieldFormulaButton :: CheckButton
                             , confFileSaveCheckButton :: CheckButton
                             , saveAsDialog :: FileChooserDialog
                             , confFileLoadCheckButton :: CheckButton
                             , loadFileDialog :: FileChooserDialog
                             , importFieldsFromFileDialog :: FileChooserDialog
                             , importFieldsInputSeparator :: Entry
                             , importFieldsOptionsDialog :: Dialog
                             , importFieldsOptionsRows :: Grid
                             , targetList :: TargetList
                             , searchFieldDialog :: Dialog
                             , searchFieldCombo :: ComboBox
                             , copyOtherDialog :: Dialog
                             , copyOtherCombo :: ComboBox
                             , textBufferConnections :: IORef [ ConnectId TextBuffer ]
                             }

sendInput :: IsInput cmd => GUIControl -> cmd -> IO ()
sendInput control = writeChan (inputChan control) . toInput