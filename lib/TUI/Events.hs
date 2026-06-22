{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TUI.Events (
    EventType
    , BackupEvent(..)
    , handleEvent
    ) where



import Brick hiding (getName, zoom)
import Brick qualified as B
import Brick.Widgets.Edit qualified as Ed
import Control.Exception (catch)
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Either.Combinators (leftToMaybe)
import Data.Text qualified as T
import Data.Text.Zipper (insertMany)
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Button (..))
import Model.Expression.RecursionSchemas
import Model.RowStore
import System.Process (readProcess)

import TUI.Base
import TUI.Level
import TUI.State
import Graphics.Vty (Modifier(..))

data BackupEvent = BackupEvent deriving (Show)

type EventType = BackupEvent

(>>->>) :: EventM Name State Bool -> EventM Name State () -> EventM Name State ()
(>>->>) e1 e2 = e1 >>= \case
                    True -> return ()
                    False -> e2

handleEvent :: BrickEvent Name EventType -> EventM Name State ()
handleEvent (AppEvent BackupEvent) = doBackup
handleEvent e = do
    case e of
        MouseDown {} -> logMessage $ "Mouse " <> T.pack (show e)
        VtyEvent k@(EvKey {}) -> logMessage $ "Key " <> T.pack (show k)
        _ -> return ()
    handleGlobalEvent e >>->> (use sInterface >>= handleInLevel e . out)

handleInLevel :: BrickEvent Name EventType -> Level Interface -> EventM Name State ()
handleInLevel e (WithDialog dl _) = handleEventDialogLevel dl e
handleInLevel e (Zoomed zl i) = handleEventZoomLevel zl e >>->> handleInLevel e (out i)
handleInLevel e (Back bl) = handleEventBackLevel bl e

handleGlobalEvent :: BrickEvent Name EventType -> EventM Name State Bool
handleGlobalEvent (VtyEvent (EvKey (KChar 'q') [MCtrl])) = do
    rst <- use sRowStore
    if changed rst
    then sInterface . quitDialog .= Just (mkMessageDialog "Are you sure you want to quit?" "Quit" True)
    else doQuit
    return True
handleGlobalEvent (VtyEvent (EvKey (KChar 'w') [MCtrl])) = doSave >> return True
handleGlobalEvent _ = return False

handleEventDialogLevel :: DialogLevel -> BrickEvent Name EventType -> EventM Name State ()
handleEventDialogLevel (Searching dl) ev = do
    res <- B.zoom (sInterface . searchDialog . anon dl (const False)) $ handleEventSearchDialog ev
    case res of
        DoNothing -> return ()
        DialogCancel -> deactivateSearch
        DialogResult t -> searchSelection t
handleEventDialogLevel (Quitting dl) ev = do
    res <- B.zoom (sInterface . quitDialog . anon dl (const False)) $ handleEventMessageDialog ev
    case res of
        DoNothing -> return ()
        DialogCancel -> abortQuit
        DialogResult () -> doQuit
handleEventDialogLevel (Informing dl) ev = do
    res <- B.zoom (sInterface . messageDialog . anon dl (const False)) $ handleEventMessageDialog ev
    case res of
        DoNothing -> return ()
        _ -> sInterface . messageDialog .= Nothing
handleEventDialogLevel (FieldProperties dl) ev = do
    res <- B.zoom (sInterface . fieldProperties . anon dl (const False)) $ handleEventFieldPropertiesDialog ev
    case res of
        DoNothing -> return ()
        DialogCancel -> closeFieldPropertiesDialog
        DialogResult t -> acceptFieldProperties t

searchSelection :: T.Text -> EventM Name State ()
searchSelection t = do
    deactivateSearch
    s <- get
    let pos = nextPos (fromIntegral $ s ^. sCurrentField) t (s ^. sIndex) (s ^. sRowStore)
    modify $ moveTo pos

closeFieldPropertiesDialog :: EventM Name State ()
closeFieldPropertiesDialog = sInterface . fieldProperties .= Nothing

acceptFieldProperties :: FieldPropertiesInfo -> EventM Name State ()
acceptFieldProperties t = do
    changeCurrentFieldName $ t ^. fpiName
    changeCurrentFieldType $ t ^. fpiType
    changeCurrentFieldFormula . leftToMaybe $ t ^. fpiFormulaOrValue
    case t ^. fpiFormulaOrValue of
        Left f -> changeCurrentFieldFormula $ Just f
        Right v -> do
                      changeCurrentFieldFormula Nothing
                      updateCurrentField v
    closeFieldPropertiesDialog

doQuit :: EventM Name State ()
doQuit = doFinalBackup >> halt

abortQuit :: EventM Name State ()
abortQuit = sInterface . quitDialog .= Nothing

handleEventZoomLevel :: ZoomLevel -> BrickEvent Name EventType -> EventM Name State Bool
handleEventZoomLevel _ (MouseDown {}) = return True
handleEventZoomLevel (NormalZoom _) _ = return False

handleCommonKeys :: BrickEvent Name EventType -> EventM Name State Bool
handleCommonKeys (VtyEvent (EvKey (KChar c) [m]))
  | m == MAlt || m == MMeta = case c of
    'r' -> toggleProperties >> return True
    'z' -> toggleZoom >> return True
    _ -> return False
  | m == MCtrl = case c of
    'f' -> activateSearch >> return True
    'r' -> do
              redo >>= \case
                True -> return ()
                False -> sInterface . messageDialog .= Just (mkMessageDialog "Nothing to redo" "Redo" False)
              return True
    't' -> toggleTable >> return True
    'z' -> do
              undo >>= \case
                True -> return ()
                False -> sInterface . messageDialog .= Just (mkMessageDialog "Nothing to undo" "Undo" False)
              return True
    '\t' -> forward >> return True
    _ -> return False
handleCommonKeys (VtyEvent (EvKey k [MCtrl])) = case k of
    KUp -> backward >> return True
    KDown -> forward >> return True
    KLeft -> fieldBackward >> return True
    KRight -> fieldForward >> return True
    _ -> return False
handleCommonKeys (VtyEvent (EvKey k [])) = case k of
    KChar '\t' -> fieldForward >> return True
    KBackTab -> fieldBackward >> return True
    _ -> return False
handleCommonKeys _ = return False

handleEventBackLevel :: BackLevel -> BrickEvent Name EventType -> EventM Name State ()
handleEventBackLevel (AsTable _) e = handleCommonKeys e >>->> handleEventTable e
handleEventBackLevel (AsRows _) e = handleCommonKeys e >>->> handleEventRows e

handleEventTable :: BrickEvent Name EventType -> EventM Name State ()
handleEventTable e = case e of
    VtyEvent (EvKey KUp []) -> backward
    VtyEvent (EvKey KDown []) -> forward
    VtyEvent (EvKey KLeft []) -> fieldBackward
    VtyEvent (EvKey KRight []) -> fieldForward
    MouseDown (ValueColumn c) BLeft [] (Location (_, r)) -> modify $ moveFieldTo c . moveTo r
    MouseDown (ValueColumn _) BScrollUp [] _ -> backward
    MouseDown (ValueColumn _) BScrollDown [] _ -> forward
    _ -> return ()


handleEventRows :: BrickEvent Name EventType -> EventM Name State ()
handleEventRows e = case e of
    VtyEvent (EvKey KPageUp []) -> backward
    VtyEvent (EvKey KPageDown []) -> forward
    VtyEvent (EvKey KUp []) -> fieldBackward
    VtyEvent (EvKey KDown []) -> fieldForward
    VtyEvent (EvKey KEnter []) -> fieldForward
    VtyEvent (EvKey (KChar 'n') [MCtrl]) -> newRow
    MouseDown FieldNames BLeft [] (Location (_, r)) -> modify $ moveFieldTo r
    MouseDown ValueList BLeft [] (Location (_, r)) -> modify $ moveFieldTo r
    MouseDown (ValueViewer r) BLeft [] _ -> modify (moveFieldTo r) >> handleEdition e
    MouseDown (ValueViewer r) BMiddle [] _ -> modify (moveFieldTo r) >> handleEdition e
    _ -> handleEdition e

handleEdition :: BrickEvent Name EventType -> EventM Name State ()
handleEdition e = do
    rst <- use sRowStore
    unless (nFields rst == 0) $ do
        isF <- gets isFormulaCurrentField
        when (not isF) $ do
            B.zoom (sInterface . activeEditor . _Just) $ handleEventValueEditor e
            use (sInterface . activeEditor) >>= \case
                Nothing -> return ()
                Just ve -> updateCurrentField $ ve ^. veField

handleEventValueEditor :: BrickEvent Name EventType -> EventM Name ValueEditor ()
handleEventValueEditor ev = do
    case ev of
         MouseDown (ValueViewer _) BMiddle [] _ -> copyClipboard True
         VtyEvent (EvKey (KChar 'v') [MCtrl]) -> copyClipboard False
         _ -> B.zoom veEditor $ Ed.handleEditorEvent ev
    ve <- get
    let f = convertKeepText (ve ^. veType) (toField $ ve ^. veContent)
    modify $ updateEditor f

copyClipboard :: Bool -> EventM Name ValueEditor ()
copyClipboard isPrimary = do
    let options = if isPrimary then ["-o"] else ["-selection", "clipboard", "-o"]
    cl <- liftIO $ readProcess "xclip" options "" `catch` \(_ :: IOError) -> return ""
    veEditor %= Ed.applyEdit (insertMany $ T.pack cl)
