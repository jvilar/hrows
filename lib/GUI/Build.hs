{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

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
import Data.BitVector(nil)
import Data.Functor.Identity(Identity)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Maybe(isJust, fromJust)
import Data.Text(Text)
import qualified Data.Text as T
import GHC.Generics(Generic, K1(..), M1(..), Rep(..), V1(..), U1(..)
                   , (:*:)(..), (:+:)(..), from, to)

import GI.Gdk (screenGetDefault, EventKey, keyvalName, ModifierType(ModifierTypeControlMask))
import GI.Gtk hiding (MessageDialog)

import Paths_hrows(getDataFileName)

import GUI.Control
import GUI.Iteration
import Model.Field
import Presenter.ImportType
import Presenter.Input

makeGUI :: Chan Input -> IO GUIControl
makeGUI iChan = do
  GI.Gtk.init Nothing

  builder <- builderNew
  gladefn <- getDataFileName "src/hrows.glade"
  builderAddFromFile builder $ T.pack gladefn

  styleFile <- getDataFileName "src/hrows.css"

  provider <- cssProviderNew
  cssProviderLoadFromPath provider $ T.pack styleFile
  Just screen <- screenGetDefault
  styleContextAddProviderForScreen screen provider $ fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION

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

class GObject a => CanBeCast a where
    doCast :: GObject o => o -> IO a

instance CanBeCast Button where
    doCast = unsafeCastTo Button

instance CanBeCast CheckButton where
    doCast = unsafeCastTo CheckButton

instance CanBeCast ComboBox where
    doCast = unsafeCastTo ComboBox

instance CanBeCast ComboBoxText where
    doCast = unsafeCastTo ComboBoxText

instance CanBeCast Dialog where
    doCast = unsafeCastTo Dialog

instance CanBeCast Entry where
    doCast = unsafeCastTo Entry

instance CanBeCast FileChooserDialog where
    doCast = unsafeCastTo FileChooserDialog

instance CanBeCast Grid where
    doCast = unsafeCastTo Grid

instance CanBeCast Label where
    doCast = unsafeCastTo Label

instance CanBeCast Menu where
    doCast = unsafeCastTo Menu

instance CanBeCast MenuItem where
    doCast = unsafeCastTo MenuItem

instance CanBeCast Window where
    doCast = unsafeCastTo Window

getBuilder :: BuildMonad Builder
getBuilder = asks fst

getControl :: BuildMonad GUIControl
getControl = asks snd

getMainWindow :: BuildMonad Window
getMainWindow = mainWindow <$> getControl

getObject :: CanBeCast obj => Text -> BuildMonad obj
getObject s = do
    builder <- getBuilder
    liftIO $ builderGetObject builder s >>= doCast . fromJust

ioVoid :: IO a -> BuildMonad ()
ioVoid = liftIO . void

buttonAction :: IsInput cmd => Text -> cmd -> BuildMonad ()
buttonAction name input = do
    control <- getControl
    btn <- getObject name
    ioVoid ((btn :: Button) `on` #clicked $ sendInput control input)

buttons :: IsInput cmd => [(Text, cmd)] -> BuildMonad ()
buttons = mapM_ (uncurry buttonAction)

menuItemInput :: IsInput cmd => Text -> cmd -> BuildMonad ()
menuItemInput name input = do
    control <- getControl
    menuItemAction name $ sendInput control input

menuItemAction :: Text -> IO () -> BuildMonad ()
menuItemAction name io = do
    control <- getControl
    itm <- getObject name
    ioVoid ((itm :: MenuItem) `on` #activate $ io)

fieldMenuAction :: IsInput cmd => Text -> (FieldPos -> cmd) -> BuildMonad ()
fieldMenuAction name f = do
    control <- getControl
    menuItemAction name $ (f <$> readIORef (currentField control)) >>=
                            sendInput control

prepareControl :: Chan Input -> Builder -> IO GUIControl
prepareControl iChan builder = do
  let getObject :: CanBeCast obj => Text -> IO obj
      getObject name = builderGetObject builder name >>= doCast . fromJust
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
    , importRowsOptionsDialog = getObject "importRowsOptionsDialog"
    , importRowsOptionsRows = getObject "importRowsOptionsRows"
    , targetList = targetListNew (Just [])
    , searchFieldDialog = getObject "searchFieldDialog"
    , searchFieldCombo = getObject "searchFieldCombo"
    , copyOtherDialog = getObject "copyOtherDialog"
    , copyOtherCombo = getObject "copyOtherCombo"
    , textBufferActive = newIORef nil
    }
  targetListAddTextTargets (targetList control) 0
  return control

globalKeys = [ (("Page_Down", []), toInput MoveNext)
             , (("Page_Up", []), toInput MovePrevious)
             , (("q", [ModifierTypeControlMask]), toInput ExitRequested)
             , (("r", [ModifierTypeControlMask]), toInput Redo)
             , (("z", [ModifierTypeControlMask]), toInput Undo)
             , (("Return", []), toInput DoNothing)
             ]

prepareMainWindow :: BuildMonad ()
prepareMainWindow = do
  window <- getMainWindow
  control <- getControl
  liftIO $ do
      window `on` #deleteEvent $ const $ do
        liftIO $ sendInput control ExitRequested
        return False
      window `on` #keyPressEvent $ \evk -> do
          name <- get evk #keyval >>= keyvalName
          mods <- get evk #state
          -- showEvent evk
          let cmd = do
                n <- name
                lookup (n, mods) globalKeys
          -- liftIO $ print cmd
          maybe (return False)
                (\c -> liftIO $ sendInput control c >> return True)
                cmd
      widgetShowAll window

showEvent :: EventKey -> BuildMonad()
showEvent evk = do
  name <- get evk #keyval >>= keyvalName
  mods <- get evk #state
  liftIO $ putStrLn $ "Key name: " ++ show name
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
                             ,("importFieldsMenuItem", toInput $ ImportFromDialog ImportFields)
                             ,("importRowsMenuItem", toInput $ ImportFromDialog ImportRows)
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

{-
gray :: Color
gray = Color 53000 53000 53000
-}

prepareChangeFieldFormulaDialog :: BuildMonad ()
prepareChangeFieldFormulaDialog = do
    control <- getControl
    let btn = changeFieldFormulaButton control
        entry = changeFieldFormulaEntry control
    {- TODO 
    ioVoid $ widgetModifyBg entry StateInsensitive gray -}
    ioVoid $ btn `on` #toggled $ toggleButtonGetActive btn >>=
                                 widgetSetSensitive entry


notImplementedDialog :: Text -> Input
notImplementedDialog f = toInput $ MessageDialog (InformationMessage  $ T.concat ["Opción ", f, " no implementada"])

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
