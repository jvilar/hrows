{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}

module GUI.MainWindow.Build (
  -- *Functions
  buildMainWindow
  , configureMainWindow
  ) where

import Control.Concurrent.Chan(Chan)
import Control.Monad((>=>))
import Control.Monad.IO.Class(liftIO)
import Data.BitVector(nil)
import Data.IORef(newIORef, readIORef)
import Data.Maybe(fromJust)
import Data.Text(Text)
import GI.Gtk
import GI.Gdk(keyvalName, EventKey, ModifierType(..))

import Model.Field
import Presenter.Input

import GUI.Control
import GUI.CanBeCast
import GUI.BuildMonad
import GUI.HKD
import GUI.MainWindow

buildMainWindow :: Chan Input -> Builder -> IO MainWindow
buildMainWindow iChan builder = do
    let
      getObject :: CanBeCast obj => Text -> IO obj
      getObject name = builderGetObject builder name >>= doCast . fromJust
    mw <- fromIO MainWindow {
              window = getObject "mainWindow"
              , positionLabel = getObject "positionLabel"
              , fieldsGrid = getObject "fieldsGrid"
              , numberOfFields = newIORef 0
              , currentField = newIORef 0
              , fieldMenu = getObject "fieldPopupMenu"
              , beginButton = getObject "beginButton"
              , endButton =  getObject "endButton"
              , leftButton = getObject "leftButton"
              , rightButton = getObject "rightButton"
              , targetList = targetListNew (Just [])
              , textBufferActive = newIORef nil
              , inputChanMW = return iChan
              }
    targetListAddTextTargets (targetList mw) 0
    return mw

configureMainWindow :: BuildMonad ()
configureMainWindow = do
                prepareMainWindow
                prepareMovementButtons
                prepareRecordButtons
                prepareQuitButton
                prepareFileMenu
                prepareFieldMenu
                prepareRecordMenu
                prepareListingMenu
                prepareSourceMenu
                prepareHelpMenu

globalKeys :: [ ((Text, [ModifierType]), Input)]
globalKeys = [ (("Page_Down", []), toInput MoveNext)
             , (("KP_Next", []), toInput MoveNext)
             , (("Page_Up", []), toInput MovePrevious)
             , (("KP_Page_Up", []), toInput MovePrevious)
             , (("q", [ModifierTypeControlMask]), toInput ExitRequested)
             , (("r", [ModifierTypeControlMask]), toInput Redo)
             , (("z", [ModifierTypeControlMask]), toInput Undo)
             , (("Return", []), toInput DoNothing)
             ]

ignoredModfifiers :: [ModifierType]
ignoredModfifiers = [ModifierTypeLockMask, ModifierTypeMod2Mask]

commandFromGlobalKey :: EventKey -> IO (Maybe Input)
commandFromGlobalKey evk = do
  n <- get evk #keyval >>= keyvalName
  case n of
    Just name -> do
      mods <- filter (`notElem` ignoredModfifiers) <$> get evk #state
      return $ lookup (name, mods) globalKeys
    Nothing -> return Nothing

prepareMainWindow :: BuildMonad ()
prepareMainWindow = do
  control <- getControl
  w <- window <$> getMainWindow
  liftIO $ do
    _ <- w `on` #deleteEvent $ const $ do
      liftIO $ sendInput control ExitRequested
      return True
    _ <- w `on` #keyPressEvent $
      (commandFromGlobalKey >=>
       (\case
          Nothing -> return False
          Just cmd -> do
            liftIO $ sendInput control cmd
            return True))
    widgetShowAll w

showEvent :: EventKey -> IO()
showEvent evk = do
  name <- get evk #keyval >>= keyvalName
  mods <- get evk #state
  putStrLn $ "Key name: " ++ show name
  putStrLn $ "Modifiers: " ++ show mods

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
                             ,("changeNamesMenuItem", toInput RenameFieldsDialog)
                             ,("showHideMenuItem", toInput ShowHideFieldsDialog)
                             ]

prepareFieldMenu :: BuildMonad ()
prepareFieldMenu = do
                     fieldMenuAction "searchFieldMenuItem" SearchFieldDialog
                     fieldMenuAction "copyOtherMenuItem" CopyOtherDialog
                     fieldMenuAction "deleteFieldMenuItem" (DeleteFields . (:[]))
                     fieldMenuAction "hideFieldMenuItem" HideField
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

prepareListingMenu :: BuildMonad ()
prepareListingMenu = menuItemInput "newListingMenuItem" (toInput ShowListingRequested)

prepareSourceMenu :: BuildMonad ()
prepareSourceMenu = mapM_ (uncurry menuItemInput)
                               [("addSourceMenuItem", toInput AddSourceDialog)
                               , ("showSourcesMenuItem", toInput ShowSourcesDialog)
                               , ("deleteSourceMenuItem", toInput DeleteSourceDialog)
                               , ("renameSourcesMenuItem", toInput RenameSourcesDialog)
                               ]

prepareHelpMenu :: BuildMonad ()
prepareHelpMenu = mapM_ (uncurry menuItemInput)
                               [("aboutMenuItem", toInput ShowAboutDialog)
                               ]

fieldMenuAction :: IsInput cmd => Text -> (FieldPos -> cmd) -> BuildMonad ()
fieldMenuAction name f = do
    control <- getControl
    menuItemAction name $ (f <$> readIORef (currentField $ mainWindow control)) >>=
                            sendInput control
