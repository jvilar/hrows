{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI.Control (
            -- *Types
            GUIControl
            , GUIControl'(..)
            -- *Functions
            , sendInput
) where

import Control.Concurrent.Chan(Chan, writeChan)
import Data.Functor.Identity(Identity)
import Data.IORef(IORef)
import GHC.Generics(Generic)
import Graphics.UI.Gtk

import Presenter.Input

import GUI.HKD

data GUIControl' f = GUIControl { mainWindow :: HKD f Window
                                , positionLabel :: HKD f Label
                                , fieldsGrid :: HKD f Grid
                                , numberOfFields :: HKD f (IORef Int)
                                , currentField :: HKD f (IORef Int)
                                , inputChan :: HKD f (Chan Input)
                                , fieldMenu :: HKD f Menu
                                , beginButton :: HKD f Button
                                , endButton :: HKD f Button
                                , leftButton :: HKD f Button
                                , rightButton :: HKD f Button
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
                                , targetList :: HKD f TargetList
                                , searchFieldDialog :: HKD f Dialog
                                , searchFieldCombo :: HKD f ComboBox
                                , copyOtherDialog :: HKD f Dialog
                                , copyOtherCombo :: HKD f ComboBox
                                , textBufferConnections :: HKD f (IORef [ ConnectId TextBuffer ])
                                } deriving Generic

type GUIControl = GUIControl' Identity

sendInput :: IsInput cmd => GUIControl -> cmd -> IO ()
sendInput control = writeChan (inputChan control) . toInput
