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

import Control.Monad(when)
import Data.Text(Text)
import qualified Data.Text as T
import GI.Gtk hiding (MessageDialog)
import System.FilePath(takeFileName)

import GUI.Command
import GUI.Control
import GUI.DialogManager.Actions
import GUI.MainWindow hiding (window)
import GUI.MainWindow.Update (setTextField, showFields, disableTextViews)
import GUI.ListingWindow hiding (window)
import GUI.ListingWindow.Update (showFullListing, showFieldsRow)
import GUI.View
import Model.SourceInfo
import Presenter.ImportType
import Presenter.Input

updateGUI :: GUICommand -> GUIControl -> IO ()
updateGUI (ChangeTitle title) = inBothWindows $ changeTitle title
updateGUI (ShowPosition pos size) = inBothWindows $ updatePosition pos size
updateGUI (ShowFields pos fis) = \control -> do
                                    showFields fis $ mainWindow control
                                    ifListingVisible (showFieldsRow pos fis) control
updateGUI (ShowNames names) = inBothWindows $ updateNames names
updateGUI (ShowIteration iter) = showIteration iter
updateGUI DisableTextViews = disableTextViews . mainWindow
updateGUI ShowListing = widgetShowAll . window . listingWindow
updateGUI HideListing = widgetHide . window . listingWindow
updateGUI CompleteListingWanted = \control -> ifListingVisible (const $ sendInput control CompleteListingGranted) control
updateGUI (CompleteListing fiss) = showFullListing fiss . listingWindow

ifListingVisible :: (ListingWindow -> IO ()) -> GUIControl -> IO ()
ifListingVisible f control = do
                                 let lw = listingWindow control
                                 visible <- #isVisible $ window lw
                                 when visible $ f lw

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
                            (. LoadFileFromName) . sendInput
showIteration AskWriteFile = dialogCall askWriteFile $
                             (. WriteFileFromName) . sendInput
showIteration AskCreateField = dialogCall askCreateField $
                               (. NewFields) . sendInput
showIteration (AskDeleteFields fs) = dialogCall (askDeleteFields fs) $
                                     (. DeleteFields) . sendInput
showIteration (AskImportFrom t) = dialogCall askImportFrom $
                                    \control -> sendInput control . ImportFromFile t . sourceInfoFromDialogResult
showIteration (AskImportOptions t ifs cfs rst) = dialogCall (askImportOptions t ifs cfs)
   (\control (keys, values) -> sendInput control $ case t of
                              ImportFields -> ImportFieldsFromRowStore rst keys values
                              ImportRows -> ImportRowsFromRowStore rst values)
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
showIteration AskAddSource = dialogCall askImportFrom $
                                                \control r@(fp, _, _) -> let
                                                      name = T.pack $ takeFileName fp
                                                      si = sourceInfoFromDialogResult r
                                                    in sendInput control $ AddSourceFromSourceInfo name si
showIteration (ShowSources srcs) = \control -> showSources (dialogManager control) srcs (window $ mainWindow control)
showIteration DisplayAbout = \control -> showAboutDialog (dialogManager control) (window $ mainWindow control)

showIteration it = unimplemented (T.pack $ show it)


unimplemented :: Text -> GUIControl -> IO ()
unimplemented func control = sendInput control . MessageDialog . ErrorMessage $ T.concat ["Función ", func, " no implementada"]

sourceInfoFromDialogResult :: (FilePath, Char, HeaderType) -> SourceInfo
sourceInfoFromDialogResult (fp, c, t) = let
  lti = ListatabInfo {
          ltInputSeparator = c
          , ltOutputSeparator = c
          , ltHeaderType = t
        }
  in mkSourceInfo Nothing (PathAndConf fp Nothing) lti
