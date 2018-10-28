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
                prepareRecordMenu
                prepareChangeFieldFormulaDialog
             ) (builder, control)
  return control


type BuildMonad = ReaderT (Builder, GUIControl) IO

class GObjectClass a => CanBeCast a where
    doCast :: GObject -> a

instance CanBeCast Button where
    doCast = castToButton

instance CanBeCast CheckButton where
    doCast = castToCheckButton

instance CanBeCast ComboBox where
    doCast = castToComboBox

instance CanBeCast Dialog where
    doCast = castToDialog

instance CanBeCast Entry where
    doCast = castToEntry

instance CanBeCast FileChooserDialog where
    doCast = castToFileChooserDialog

instance CanBeCast Grid where
    doCast = castToGrid

instance CanBeCast Label where
    doCast = castToLabel

instance CanBeCast Menu where
    doCast = castToMenu

instance CanBeCast MenuItem where
    doCast = castToMenuItem

instance CanBeCast Window where
    doCast = castToWindow

getBuilder :: BuildMonad Builder
getBuilder = asks fst

getControl :: BuildMonad GUIControl
getControl = asks snd

getMainWindow :: BuildMonad Window
getMainWindow = mainWindow <$> getControl

getObject :: CanBeCast obj => String -> BuildMonad obj
getObject s = do
    builder <- getBuilder
    liftIO $ builderGetObject builder doCast s

ioVoid :: IO a -> BuildMonad ()
ioVoid = liftIO . void

buttonAction :: IsInput cmd => String -> cmd -> BuildMonad ()
buttonAction name input = do
    control <- getControl
    btn <- getObject name
    ioVoid ((btn :: Button) `on` buttonActivated $ sendInput control input)

buttons :: IsInput cmd => [(String, cmd)] -> BuildMonad ()
buttons = mapM_ (uncurry buttonAction)

menuItemInput :: IsInput cmd => String -> cmd -> BuildMonad ()
menuItemInput name input = do
    control <- getControl
    menuItemAction name $ sendInput control input

menuItemAction :: String -> IO () -> BuildMonad ()
menuItemAction name io = do
    control <- getControl
    itm <- getObject name
    ioVoid ((itm :: MenuItem) `on` menuItemActivated $ io)

fieldMenuAction :: IsInput cmd => String -> (Int -> cmd) -> BuildMonad ()
fieldMenuAction name f = do
    control <- getControl
    menuItemAction name $ (f <$> readIORef (currentField control)) >>=
                            sendInput control

prepareControl :: Chan Input -> Builder -> IO GUIControl
prepareControl iChan builder = do
  let getObject :: CanBeCast obj => String -> IO obj
      getObject = builderGetObject builder doCast
  lbl <- getObject "positionLabel"
  grid <- getObject "fieldsGrid"
  window <- getObject "mainWindow"
  fmenu <- getObject "fieldPopupMenu"
  bButton <- getObject "beginButton"
  eButton <- getObject "endButton"
  lButton <- getObject "leftButton"
  rButton <- getObject "rightButton"
  fields <- newIORef 0
  cfield <- newIORef 0
  cfDialog <- getObject "changeFieldFormulaDialog"
  cfEntry <- getObject "changeFieldFormulaEntry"
  cfLabel <- getObject "changeFieldFormulaLabel"
  cfButton <- getObject "changeFieldFormulaCheckButton"
  confSButton <- getObject "confFileSaveCheckButton"
  sDialog <- getObject "saveAsDialog"
  confLButton <- getObject "confFileLoadCheckButton"
  lDialog <- getObject "loadFileDialog"
  ifDialog <- getObject "importFieldsFromFileDialog"
  ifEntry <- getObject "importFieldsInputSeparator"
  ifoDialog <- getObject "importFieldsOptionsDialog"
  ifRows <- getObject "importFieldsOptionsRows"
  tlist <- targetListNew
  targetListAddTextTargets tlist 0
  connections <- newIORef []

  sfDialog <- getObject "searchFieldDialog"
  sfCombo <- getObject "searchFieldCombo"
  comboBoxSetModelText sfCombo
  coDialog <- getObject "copyOtherDialog"
  coCombo <- getObject "copyOtherCombo"
  comboBoxSetModelText coCombo

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
                    , confFileSaveCheckButton = confSButton
                    , saveAsDialog = sDialog
                    , confFileLoadCheckButton = confLButton
                    , loadFileDialog = lDialog
                    , importFieldsFromFileDialog = ifDialog
                    , importFieldsInputSeparator = ifEntry
                    , importFieldsOptionsDialog = ifoDialog
                    , importFieldsOptionsRows = ifRows
                    , targetList = tlist
                    , searchFieldDialog = sfDialog
                    , searchFieldCombo = sfCombo
                    , copyOtherDialog = coDialog
                    , copyOtherCombo = coCombo
                    , textBufferConnections = connections
                    }

globalKeys = [ (("Page_Down", []), toInput MoveNext)
             , (("Page_Up", []), toInput MovePrevious)
             , (("q", [Control]), toInput ExitRequested)
             , (("r", [Control]), toInput Redo)
             , (("z", [Control]), toInput Undo)
             , (("Return", []), toInput DoNothing)
             ]

prepareMainWindow :: BuildMonad ()
prepareMainWindow = do
  window <- getMainWindow
  control <- getControl
  liftIO $ do
      void (window `on` deleteEvent $ do
        liftIO $ sendInput control ExitRequested
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
prepareQuitButton = buttonAction "quitButton" ExitRequested

prepareFileMenu :: BuildMonad ()
prepareFileMenu  = mapM_ (uncurry menuItemInput)
                             [("openMenuItem", toInput LoadFileDialog)
                             ,("saveMenuItem",  toInput WriteFile)
                             ,("saveAsMenuItem", toInput SaveAsFileDialog)
                             ,("quitMenuItem", toInput ExitRequested)
                             ,("createFieldsMenuItem", toInput CreateFieldsDialog)
                             ,("deleteFieldsMenuItem", toInput DeleteFieldsDialog)
                             ,("importFieldsMenuItem", toInput ImportFieldsFromDialog)
                             ,("changeNamesMenuItem", toInput ChangeNamesDialog)
                             ]

prepareFieldMenu :: BuildMonad ()
prepareFieldMenu = do
                     fieldMenuAction "searchFieldMenuItem" SearchFieldDialog
                     fieldMenuAction "copyOtherMenuItem" CopyOtherDialog
                     fieldMenuAction "deleteFieldMenuItem" (DeleteFields . (:[]))
                     fieldMenuAction "formulaMenuItem" ChangeFieldFormulaDialog
                     fieldMenuAction "changeToStringMenuItem" (ChangeFieldType TypeString)
                     fieldMenuAction "changeToIntMenuItem" (ChangeFieldType TypeInt)
                     fieldMenuAction "changeToInt0MenuItem" (ChangeFieldType TypeInt0)
                     fieldMenuAction "changeToFloatMenuItem" (ChangeFieldType TypeDouble)
                     fieldMenuAction "changeToFloat0MenuItem" (ChangeFieldType TypeDouble0)

prepareRecordMenu :: BuildMonad ()
prepareRecordMenu = mapM_ (uncurry menuItemInput)
                              [("newRowMenuItem", toInput NewRow)
                              ,("deleteRowMenuItem", toInput DeleteRow)
                              ,("sortRowsMenuItem", toInput SortRowsDialog)
                              ]


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
