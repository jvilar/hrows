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
import GUI.DialogManager
import GUI.HKD
import GUI.MainWindow

data GUIControl' f = GUIControl { mainWindow :: HKD f MainWindow
                                , dialogManager :: HKD f DialogManager
                                , inputChan :: HKD f (Chan Input)
                                } deriving Generic

type GUIControl = GUIControl' Identity

sendInput :: IsInput cmd => GUIControl -> cmd -> IO ()
sendInput control = writeChan (inputChan control) . toInput
