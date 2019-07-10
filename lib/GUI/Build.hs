{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Functor.Identity(Identity)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Maybe(isJust, fromJust)
import GHC.Generics(Generic, K1(..), M1(..), Rep(..), V1(..), U1(..)
                   , (:*:)(..), (:+:)(..), from, to)

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
  gladefn <- getDataFileName "src/hrows.glade"
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
  control <- fromIO GUIControl {
    mainWindow = getObject "mainWindow"
    , positionLabel = getObject "positionLabel"
    , fieldsGrid = getObject "fieldsGrid"
    , numberOfFields = newIORef 0
    , currentField = newIORef 0
    , inputChan = return iChan
    , fieldMenu = getObject "fieldPopupMenu"
    , beginButton = getObject "beginButton"
    , endButton =  getObject "endButton"
    , leftButton = getObject "leftButton"
    , rightButton = getObject "rightButton"
    , changeFieldFormulaDialog = getObject "changeFieldFormulaDialog"
    , changeFieldFormulaEntry = getObject "changeFieldFormulaEntry"
    , changeFieldFormulaLabel = getObject "changeFieldFormulaLabel"
    , changeFieldFormulaButton = getObject "changeFieldFormulaCheckButton"
    , confFileSaveCheckButton =  getObject "confFileSaveCheckButton"
    , saveAsDialog = getObject "saveAsDialog"
    , confFileLoadCheckButton = getObject "confFileLoadCheckButton"
    , loadFileDialog = getObject "loadFileDialog"
    , importFromFileDialog = getObject "importFromFileDialog"
    , importInputSeparator = getObject "importInputSeparator"
    , importFieldsOptionsDialog = getObject "importFieldsOptionsDialog"
    , importFieldsOptionsRows = getObject "importFieldsOptionsRows"
    , targetList = targetListNew
    , searchFieldDialog = getObject "searchFieldDialog"
    , searchFieldCombo = getObject "searchFieldCombo"
    , copyOtherDialog = getObject "copyOtherDialog"
    , copyOtherCombo = getObject "copyOtherCombo"
    , textBufferConnections = newIORef []
    }
  targetListAddTextTargets (targetList control) 0
  comboBoxSetModelText $ searchFieldCombo control
  comboBoxSetModelText $ copyOtherCombo control
  return control

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

fromIO :: GUIControl' IO -> IO GUIControl
fromIO = fmap to . gFromIO . from

class GFromIO i o where
  gFromIO :: i p -> IO (o g)

instance GFromIO (K1 a (IO k)) (K1 a k) where
  gFromIO (K1 k) = K1 <$> k

instance (GFromIO i o, GFromIO i' o')
     => GFromIO (i :*: i') (o :*: o') where
  gFromIO (l :*: r) = (:*:)
                    <$> gFromIO l
                    <*> gFromIO r
  {-# INLINE gFromIO #-}

instance (GFromIO i o, GFromIO i' o')
    => GFromIO (i :+: i') (o :+: o') where
  gFromIO (L1 l) = L1 <$> gFromIO l
  gFromIO (R1 r) = R1 <$> gFromIO r
  {-# INLINE gFromIO #-}

instance GFromIO i o => GFromIO (M1 _a _b i) (M1 _a' _b' o) where
  gFromIO (M1 x) = M1 <$> gFromIO x
  {-# INLINE gFromIO #-}

instance GFromIO V1 V1 where
  gFromIO = undefined
  {-# INLINE gFromIO #-}

instance GFromIO U1 U1 where
  gFromIO U1 = return U1
  {-# INLINE gFromIO #-}
