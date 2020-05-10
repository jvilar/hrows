{-# LANGUAGE DeriveGeneric #-}

module GUI.Control (
            -- *Types
            GUIControl
            , GUIControl'(..)
            -- *Functions
            , sendInput
) where

import Control.Concurrent.Chan(Chan, writeChan)
import GHC.Generics(Generic)

import Presenter.Input

import GUI.DialogManager
import GUI.HKD
import GUI.MainWindow
import GUI.ListingWindow

data GUIControl' f = GUIControl { mainWindow :: HKD f MainWindow
                                , listingWindow :: HKD f ListingWindow
                                , dialogManager :: HKD f DialogManager
                                , inputChan :: HKD f (Chan Input)
                                } deriving Generic

type GUIControl = GUIControl' Identity

sendInput :: IsInput cmd => GUIControl -> cmd -> IO ()
sendInput control = writeChan (inputChan control) . toInput
