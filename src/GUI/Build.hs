{-# LANGUAGE OverloadedStrings #-}

module GUI.Build (
            -- *Types
            GUIControl
            -- *Functions
            , makeGUI
) where

import Control.Concurrent.Chan(Chan, writeChan)
import Control.Monad(forM_, void, when)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(asks, ReaderT, runReaderT)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Maybe(isJust, fromJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums(Align(..))

import Paths_hrows(getDataFileName)

import GUI.Control
import GUI.Iteration
import Input

makeGUI :: Chan Input -> IO GUIControl
makeGUI iChan = do
  initGUI

  builder <- builderNew
  gladefn <- getDataFileName "hrows.glade"
  builderAddFromFile builder gladefn

  control <- prepareControl iChan builder
  runReaderT (do
                prepareMainWindow
                prepareMovementButtons
                prepareRecordButtons
                prepareQuitButton
                prepareFileMenu
             ) (builder, control)
  return control


type BuildMonad = ReaderT (Builder, GUIControl) IO

getObject :: GObjectClass obj => (GObject -> obj) -> String -> BuildMonad obj
getObject cast s = do
    builder <- asks fst
    liftIO $ builderGetObject builder cast s

getMainWindow :: BuildMonad Window
getMainWindow = asks $ mainWindow . snd

getControl :: BuildMonad GUIControl
getControl = asks snd

ioVoid :: IO a -> BuildMonad ()
ioVoid = liftIO . void

buttonAction :: IsInput cmd => String -> cmd -> BuildMonad ()
buttonAction name input = do
    control <- getControl
    btn <- getObject castToButton name
    ioVoid (btn `on` buttonActivated $ sendInput control input)

buttons :: IsInput cmd => [(String, cmd)] -> BuildMonad ()
buttons = mapM_ (uncurry buttonAction)


menuItemAction :: String -> Input -> BuildMonad ()
menuItemAction name input = do
    control <- getControl
    itm <- getObject castToMenuItem name
    ioVoid (itm `on` menuItemActivated $ sendInput control input)

prepareControl :: Chan Input -> Builder -> IO GUIControl
prepareControl iChan builder = do
  let object cast s = builderGetObject builder cast (s :: String)
  lbl <- object castToLabel "positionLabel"
  grid <- object castToGrid "rowsGrid"
  window <- object castToWindow "mainWindow"
  rows <- newIORef 0
  return GUIControl { mainWindow = window
                    , positionLabel = lbl
                    , rowsGrid = grid
                    , currentRows = rows
                    , inputChan = iChan
                    }

globalKeys = [ (("Page_Down", []), toInput MoveNext)
             , (("Page_Up", []), toInput MovePrevious)
             , (("q", [Control]), toInput ExitProgram)
             ]

prepareMainWindow :: BuildMonad ()
prepareMainWindow = do
  window <- getMainWindow
  control <- getControl
  liftIO $ do
      void (window `on` deleteEvent $ do
        liftIO $ sendInput control ExitProgram
        return False)
      void (window `on` keyPressEvent $ do
          name <- eventKeyName
          mods <- eventModifier
          -- showEvent
          let cmd = lookup (name, mods) globalKeys
          liftIO $ print cmd
          maybe (return False)
                (\c -> liftIO $ sendInput control c >> return True)
                cmd
       )
      widgetShowAll window

showEvent = do
  name <- eventKeyName
  liftIO $ putStrLn $ "Key name: " ++ show name
  mods <- eventModifier
  liftIO $ putStrLn $ "Modifiers: " ++ show mods

prepareMovementButtons :: BuildMonad ()
prepareMovementButtons = buttons
                         [ ("beginButton", MoveBegin)
                         , ("endButton", MoveEnd)
                         , ("leftButton", MovePrevious)
                         , ("rightButton", MoveNext)
                         ]

prepareRecordButtons :: BuildMonad ()
prepareRecordButtons = buttons
                       [ ("newButton", NewRow)
                       , ("deleteButton", DeleteRow)
                       ]

prepareQuitButton :: BuildMonad ()
prepareQuitButton = buttonAction "quitButton" ExitProgram

prepareFileMenu :: BuildMonad ()
prepareFileMenu  = mapM_ (uncurry menuItemAction)
                             [("openMenuItem", toInput LoadFileDialog)
                             ,("saveMenuItem",  toInput WriteFile)
                             ,("saveAsMenuItem", toInput SaveAsFileDialog)
                             ,("quitMenuItem", toInput ExitProgram)
                             ]

