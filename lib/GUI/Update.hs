{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

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
import GI.Gdk hiding (Window)
import TextShow(TextShow(showt))

import GUI.Command
import GUI.Control
import GUI.DialogManager.Actions
import GUI.MainWindow hiding (window)
import GUI.MainWindow.Update (updatePosition, setTextField, showFields, disableTextViews)
import GUI.ListingWindow hiding (window)
import GUI.View
import Model hiding (deleteFields)
import Model.DefaultFileNames
import Presenter.ImportType
import Presenter.Input

updateGUI :: GUICommand -> GUIControl -> IO ()
updateGUI (ChangeTitle title) = inBothWindows $ changeTitle title
updateGUI (ShowPosition pos size) = updatePosition pos size . mainWindow
updateGUI (ShowFields fis) = showFields fis . mainWindow
updateGUI (ShowNames names) = inBothWindows $ updateNames names
updateGUI (ShowIteration iter) = showIteration iter
updateGUI DisableTextViews = disableTextViews . mainWindow
updateGUI ShowListing = widgetShowAll . window . listingWindow
updateGUI HideListing = widgetHide . window . listingWindow

inBothWindows :: (forall v. View v => v -> IO()) -> GUIControl -> IO ()
inBothWindows f control = do
                             f $ mainWindow control
                             f $ listingWindow control 

dndError :: GUIControl -> IO ()
dndError control = sendInput control $ MessageDialog (ErrorMessage "Algo está mal en el dnd")

dialogCall :: DialogFunction t -> (GUIControl -> DialogAction t) -> GUIControl -> IO ()
dialogCall dlg action control = dlg (dialogManager control)
                                    (action control)
                                    ((window :: MainWindow -> Window) $ mainWindow control)

showIteration :: Iteration -> GUIControl -> IO ()
showIteration AskReadFile = dialogCall askReadFile $
                            (. uncurry LoadFileFromName) . sendInput
showIteration AskWriteFile = dialogCall askWriteFile $
                             (. uncurry WriteFileFromName) . sendInput
showIteration AskCreateField = dialogCall askCreateField $
                               (. NewFields) . sendInput
showIteration (AskDeleteFields fs) = dialogCall (askDeleteFields fs) $
                                     (. DeleteFields) . sendInput
showIteration (AskImportFrom t) = dialogCall askImportFrom $
                (. uncurry (ImportFromFileName t)) . sendInput
showIteration (AskImportOptions t ifs cfs m) = dialogCall (askImportOptions t ifs cfs m)
   (\control (keys, values) -> sendInput control $ case t of
                              ImportFields -> ImportFieldsFromModel m keys values
                              ImportRows -> ImportRowsFromModel m values)
showIteration (AskRenameFields fs) = dialogCall (askRenameFields fs) $
                                     (. RenameFields) . sendInput
showIteration (AskSortRows fs) = dialogCall (askSortRows fs) $
                (. uncurry SortRows) . sendInput
showIteration (DisplayMessage m) = displayMessage m . (window :: MainWindow -> Window) . mainWindow
showIteration (ConfirmExit changed) = dialogCall (confirmExit changed)
   (\control save -> do
       when save $ sendInput control WriteFile
       sendInput control ExitProgram
       mainQuit
   )
showIteration (GetFieldFormula fpos flabel ms) = dialogCall (getFieldFormula fpos flabel ms)
                                                 (\control mf -> sendInput control $ ChangeFieldFormula mf fpos)
showIteration (SearchField fpos initial l) = dialogCall (searchField fpos initial l) $
                                             (. MoveToValue fpos) . sendInput
showIteration (CopyOtherField fpos initial l) = dialogCall (copyOther fpos initial l)
                                                (\control t -> setTextField fpos t $ mainWindow control)
showIteration it = unimplemented (T.pack $ show it)


unimplemented :: Text -> GUIControl -> IO ()
unimplemented func control = sendInput control . MessageDialog . ErrorMessage $ T.concat ["Función ", func, " no implementada"]
