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
import Data.BitVector(BitVector)
import GHC.Generics(Generic)
import GI.Gdk.Structs.RGBA(RGBA)
import GI.Gtk

import Presenter.Input

import GUI.Command
import GUI.HKD

data GUIControl' f = GUIControl { mainWindow :: HKD f Window
                                , positionLabel :: HKD f Label
                                , fieldsGrid :: HKD f Grid
                                , numberOfFields :: HKD f (IORef Int)
                                , currentField :: HKD f (IORef FieldPos)
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
                                , searchFieldCombo :: HKD f ComboBoxText
                                , copyOtherDialog :: HKD f Dialog
                                , copyOtherCombo :: HKD f ComboBoxText
                                , textBufferActive :: HKD f (IORef BitVector)
                                , errorColor :: HKD f RGBA
                                , formulaColor :: HKD f RGBA
                                , emptyColor :: HKD f RGBA
                                , normalColor :: HKD f RGBA
                                } deriving Generic

type GUIControl = GUIControl' Identity

sendInput :: IsInput cmd => GUIControl -> cmd -> IO ()
sendInput control = writeChan (inputChan control) . toInput
