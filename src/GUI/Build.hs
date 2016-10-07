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
import Model.Field
import Presenter.Input

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
                prepareFieldMenu
                prepareChangeFieldFormulaDialog
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

menuItemInput :: IsInput cmd => String -> cmd -> BuildMonad ()
menuItemInput name input = do
    control <- getControl
    menuItemAction name $ sendInput control input

menuItemAction :: String -> IO () -> BuildMonad ()
menuItemAction name io = do
    control <- getControl
    itm <- getObject castToMenuItem name
    ioVoid (itm `on` menuItemActivated $ io)

fieldMenuAction :: IsInput cmd => String -> (Int -> cmd) -> BuildMonad ()
fieldMenuAction name f = do
    control <- getControl
    menuItemAction name $ (f <$> readIORef (currentField control)) >>=
                            sendInput control

prepareControl :: Chan Input -> Builder -> IO GUIControl
prepareControl iChan builder = do
  let getObject :: GObjectClass obj => (GObject -> obj) -> String -> IO obj
      getObject = builderGetObject builder
  lbl <- getObject castToLabel "positionLabel"
  grid <- getObject castToGrid "fieldsGrid"
  window <- getObject castToWindow "mainWindow"
  fmenu <- getObject castToMenu "fieldPopupMenu"
  bButton <- getObject castToButton "beginButton"
  eButton <- getObject castToButton "endButton"
  lButton <- getObject castToButton "leftButton"
  rButton <- getObject castToButton "rightButton"
  fields <- newIORef 0
  cfield <- newIORef 0
  cfDialog <- getObject castToDialog "changeFieldFormulaDialog"
  cfEntry <- getObject castToEntry "changeFieldFormulaEntry"
  cfLabel <- getObject castToLabel "changeFieldFormulaLabel"
  cfButton <- getObject castToCheckButton "changeFieldFormulaCheckButton"
  return GUIControl { mainWindow = window
                    , positionLabel = lbl
                    , fieldsGrid = grid
                    , numberOfFields = fields
                    , currentField = cfield
                    , inputChan = iChan
                    , fieldMenu = fmenu
                    , beginButton = bButton
                    , endButton = eButton
                    , leftButton = lButton
                    , rightButton = rButton
                    , changeFieldFormulaDialog = cfDialog
                    , changeFieldFormulaEntry = cfEntry
                    , changeFieldFormulaLabel = cfLabel
                    , changeFieldFormulaButton = cfButton
                    }

globalKeys = [ (("Page_Down", []), toInput MoveNext)
             , (("Page_Up", []), toInput MovePrevious)
             , (("q", [Control]), toInput ExitProgram)
             , (("Return", []), toInput DoNothing)
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
          -- liftIO $ print cmd
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
prepareFileMenu  = mapM_ (uncurry menuItemInput)
                             [("openMenuItem", toInput LoadFileDialog)
                             ,("saveMenuItem",  toInput WriteFile)
                             ,("saveAsMenuItem", toInput SaveAsFileDialog)
                             ,("quitMenuItem", toInput ExitProgram)
                             ,("createFieldsMenuItem", toInput CreateFieldsDialog)
                             ,("deleteFieldsMenuItem", toInput DeleteFieldsDialog)
                             ]

prepareFieldMenu :: BuildMonad ()
prepareFieldMenu = do
                     fieldMenuAction "deleteFieldMenuItem" (DeleteFields . (:[]))
                     fieldMenuAction "formulaMenuItem" ChangeFieldFormulaDialog
                     fieldMenuAction "changeToStringMenuItem" (ChangeFieldType TypeString)
                     fieldMenuAction "changeToIntMenuItem" (ChangeFieldType TypeInt)
                     fieldMenuAction "changeToFloatMenuItem" (ChangeFieldType TypeDouble)

gray :: Color
gray = Color 53000 53000 53000

prepareChangeFieldFormulaDialog :: BuildMonad ()
prepareChangeFieldFormulaDialog = do
    control <- getControl
    let btn = changeFieldFormulaButton control
        entry = changeFieldFormulaEntry control
    ioVoid $ widgetModifyBg entry StateInsensitive gray
    ioVoid $ btn `on` toggled $ toggleButtonGetActive btn >>=
                                 widgetSetSensitive entry


notImplementedDialog :: String -> Input
notImplementedDialog f = toInput $ MessageDialog (InformationMessage $ "Opción " ++ f ++ " no implementada")
