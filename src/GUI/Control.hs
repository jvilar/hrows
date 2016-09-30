{-# LANGUAGE OverloadedStrings #-}

module GUI.Control (
            -- *Types
            GUIControl(..)
            -- *Functions
            , sendInput
) where

import Control.Concurrent.Chan(Chan, writeChan)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Graphics.UI.Gtk

import Presenter.Input

data GUIControl = GUIControl { mainWindow :: Window
                             , positionLabel :: Label
                             , rowsGrid :: Grid
                             , currentRows :: IORef Int
                             , inputChan :: Chan Input
                             }

sendInput :: IsInput cmd => GUIControl -> cmd -> IO ()
sendInput control = writeChan (inputChan control) . toInput
