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
import Control.Monad.Reader(ask, ReaderT, runReaderT)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Maybe(isJust, fromJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums(Align(..))

import Paths_hrows(getDataFileName)

import GUI.Control
import Input
import Iteration

makeGUI :: Chan Input -> IO GUIControl
makeGUI iChan = do
  initGUI

  builder <- builderNew
  gladefn <- getDataFileName "hrows.glade"
  builderAddFromFile builder gladefn

  runReaderT (do
                control <- prepareControl iChan

                liftIO $ prepareMainWindow control
                prepareMovementButtons control
                prepareRecordButtons control
                prepareQuitButton control
                prepareFileMenu control
                return control
             ) builder


type BuildMonad = ReaderT Builder IO

getObject :: GObjectClass obj => (GObject -> obj) -> String -> BuildMonad obj
getObject cast s = do
    builder <- ask
    liftIO $ builderGetObject builder cast s

ioVoid :: IO a -> BuildMonad ()
ioVoid = liftIO . void

buttonAction :: String -> IO () -> BuildMonad ()
buttonAction name action = do
    btn <- getObject castToButton name
    ioVoid (btn `on` buttonActivated $ action)

menuItemAction :: String -> IO () -> BuildMonad ()
menuItemAction name action = do
    itm <- getObject castToMenuItem name
    ioVoid (itm `on` menuItemActivated $ action)

prepareControl :: Chan Input -> BuildMonad GUIControl
prepareControl iChan = do
  lbl <- getObject castToLabel "positionLabel"
  grid <- getObject castToGrid "rowsGrid"
  window <- getObject castToWindow "mainWindow"
  rows <- liftIO $ newIORef 0
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

prepareMainWindow :: GUIControl -> IO ()
prepareMainWindow control = do
  let window = mainWindow control
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

prepareMovementButtons :: GUIControl -> BuildMonad ()
prepareMovementButtons control =
    mapM_ (\(name, input) -> buttonAction name $ sendInput control input)
              [ ("beginButton", MoveBegin)
              , ("endButton", MoveEnd)
              , ("leftButton", MovePrevious)
              , ("rightButton", MoveNext)
              ]

prepareRecordButtons :: GUIControl -> BuildMonad ()
prepareRecordButtons control =
    mapM_ (\(name, input) -> buttonAction name $ sendInput control input)
              [ ("newButton", NewRow)
              , ("deleteButton", DeleteRow)
              ]

prepareQuitButton :: GUIControl -> BuildMonad ()
prepareQuitButton control = buttonAction "quitButton" $ sendInput control ExitProgram

prepareFileMenu :: GUIControl -> BuildMonad ()
prepareFileMenu control = mapM_ (\(n, cmd) -> menuItemAction n $ sendInput control cmd)
                            [("openMenuItem", toInput LoadFileDialog)
                            ,("saveMenuItem",  toInput WriteFile)
                            ,("saveAsMenuItem", toInput SaveAsFileDialog)
                            ,("quitMenuItem", toInput ExitProgram)
                            ]

