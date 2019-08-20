{-# LANGUAGE OverloadedStrings
           , OverloadedLabels
#-}

module GUI.Update (
            -- *Types
            GUICommand
            -- *Functions
            , updateGUI
) where

import Control.Monad(filterM, forM, forM_, guard, unless, when)
import Control.Monad.IO.Class(liftIO)
import Data.Bits(Bits(..))
import Data.BitVector(nil, BV, extract, (#), ones, (@.))
import qualified Data.BitVector as BV
import Data.Either(lefts, rights)
import Data.IORef(IORef, modifyIORef, readIORef, writeIORef)
import Data.List(elemIndex, sort)
import Data.Maybe(catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Text(Text)
import qualified Data.Text as T
import GHC.Int(Int32)
import GI.Gtk hiding (MessageDialog)
import GI.Gdk
import TextShow(TextShow(showt))

import GUI.Command
import GUI.Control
import GUI.DialogManager.Actions
import GUI.MainWindow
import GUI.MainWindow.Update
import Model hiding (deleteFields)
import Model.DefaultFileNames
import Presenter.ImportType
import Presenter.Input

updateGUI :: GUICommand -> GUIControl -> IO ()
updateGUI (ChangeTitle title) = changeTitle title . mainWindow
updateGUI (ShowPosition pos size) = updatePosition pos size . mainWindow
updateGUI (ShowFields fis) = showFields fis . mainWindow
updateGUI (ShowNames names) = updateNames names . mainWindow
updateGUI (ShowIteration iter) = showIteration iter
updateGUI DisableTextViews = disableTextViews . mainWindow

dndError :: GUIControl -> IO ()
dndError control = sendInput control $ MessageDialog (ErrorMessage "Algo está mal en el dnd")

dialogCall :: DialogFunction t -> DialogAction t -> GUIControl -> IO ()
dialogCall dlg action control = dlg (dialogManager control) action (window $ mainWindow control)

showIteration :: Iteration -> GUIControl -> IO ()
showIteration AskReadFile control = dialogCall askReadFile
                                       (sendInput control . uncurry LoadFileFromName)
                                       control
showIteration AskWriteFile control = dialogCall askWriteFile
                                       (sendInput control . uncurry WriteFileFromName)
                                       control
showIteration AskCreateField control = dialogCall askCreateField
                                       (sendInput control . NewFields)
                                       control
showIteration (AskDeleteFields fs) control = dialogCall (askDeleteFields fs)
                                       (sendInput control . DeleteFields)
                                       control
showIteration (AskImportFrom t) control = dialogCall askImportFrom
                (sendInput control . uncurry (ImportFromFileName t))
                control
showIteration (AskImportOptions t ifs cfs m) control = dialogCall (askImportOptions t ifs cfs m)
   (\(keys, values) -> sendInput control $ case t of
                              ImportFields -> ImportFieldsFromModel m keys values
                              ImportRows -> ImportRowsFromModel m values)
   control
showIteration (AskRenameFields fs) control = dialogCall (askRenameFields fs)
                                              (sendInput control . RenameFields)
                                              control
showIteration (AskSortRows fs) control = dialogCall (askSortRows fs)
                (sendInput control . uncurry SortRows)
                control
showIteration (DisplayMessage m) control = displayMessage m (window $ mainWindow control)
showIteration (ConfirmExit changed) control = dialogCall (confirmExit changed)
   (\save -> do
       when save $ sendInput control WriteFile
       sendInput control ExitProgram
       mainQuit
   )
   control

showIteration (GetFieldFormula fpos flabel ms) control = dialogCall (getFieldFormula fpos flabel ms)
                                                         (\mf -> sendInput control $ ChangeFieldFormula mf fpos)
                                                         control
showIteration (SearchField fpos initial l) control = dialogCall (searchField fpos initial l)
                                                     (sendInput control . MoveToValue fpos)
                                                     control
showIteration (CopyOtherField fpos initial l) control  = dialogCall (copyOther fpos initial l)
                                                         (\t -> setTextField fpos t $ mainWindow control)
                                                         control
showIteration it control = unimplemented (T.pack $ show it) control


unimplemented :: Text -> GUIControl -> IO ()
unimplemented func control = sendInput control . MessageDialog . ErrorMessage $ T.concat ["Función ", func, " no implementada"]
