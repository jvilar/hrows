{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.Events (
    EventType
    , BackupEvent(..)
    , handleEvent
    ) where


import Brick hiding (getName, zoom)
import Brick qualified as B
import Brick.Widgets.Dialog
import Brick.Widgets.Edit qualified as Ed
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Data.Text qualified as T
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl), Button (..))
import Model.Expression.RecursionSchemas
import Model.Field
import Model.RowStore

import TUI.Base
import TUI.Level
import TUI.State
import Brick.Widgets.List (handleListEvent)
import Data.Char (isAlpha)

data BackupEvent = BackupEvent deriving (Show)

type EventType = BackupEvent

listKeys :: [Key]
listKeys = [KDown, KUp, KPageUp, KPageDown, KHome, KEnd, KLeft, KRight]

(>>->>) :: EventM Name State Bool -> EventM Name State () -> EventM Name State ()
(>>->>) e1 e2 = e1 >>= \case
                    True -> return ()
                    False -> e2

handleEvent :: BrickEvent Name EventType -> EventM Name State ()
handleEvent (AppEvent BackupEvent) = doBackup
handleEvent e = do
    case e of
        MouseDown {} -> logMessage $ "Mouse " <> T.pack (show e)
        _ -> return ()
    handleGlobalEvent e >>->> (use sInterface >>= handleInLevel e . out)

handleInLevel :: BrickEvent Name EventType -> Level Interface -> EventM Name State ()
handleInLevel e (WithDialog dl _) = handleEventDialogLevel dl e
handleInLevel e (Zoomed zl i) = handleEventZoomLevel zl e >>->> handleInLevel e (out i)
handleInLevel e (Back bl) = handleEventBackLevel bl e

handleGlobalEvent :: BrickEvent Name EventType -> EventM Name State Bool
handleGlobalEvent (VtyEvent (EvKey (KChar 'q') [MCtrl])) = do
            sInterface . quitDialog .= Just (mkYesNoDialog "Are you sure you want to quit?" "Quit")
            return True
handleGlobalEvent (VtyEvent (EvKey (KChar 'w') [MCtrl])) = doSave >> return True
handleGlobalEvent _ = return False

handleEventDialogLevel :: DialogLevel -> BrickEvent Name EventType -> EventM Name State ()
handleEventDialogLevel (Searching _) = handleEventSearch
handleEventDialogLevel (Quitting _) = handleEventQuit

handleEventSearch :: BrickEvent Name EventType -> EventM Name State ()
handleEventSearch (VtyEvent (EvKey k ms)) = handleKeySearch k ms
handleEventSearch (MouseDown SearchList BLeft [] (Location (_, r))) = moveToSelectedSearch r
handleEventSearch (MouseDown SearchList BScrollDown [] _) = handleKeySearch KPageDown []
handleEventSearch (MouseDown SearchList BScrollUp [] _) = handleKeySearch KPageUp []
handleEventSearch (MouseDown (DButton OkButton) BLeft [] _) = moveToSelected
handleEventSearch (MouseDown (DButton CancelButton) BLeft [] _) = deactivateSearch
handleEventSearch _ = return ()

handleKeySearch :: Key -> [Modifier] -> EventM Name State ()
handleKeySearch k [] | k `elem` listKeys = handleSearchList (EvKey k [])
handleKeySearch (KChar k) [] | isAlpha k = searchLetter k
handleKeySearch KEnter [] = moveToSelected
handleKeySearch KEsc [] = deactivateSearch
handleKeySearch k ms = handleInSearchDialog (EvKey k ms)

handleSearchList :: Event -> EventM Name State ()
handleSearchList e = B.zoom (sInterface . searchDialog . _Just . sdValues) $ handleListEvent e

handleEventQuit :: BrickEvent Name EventType -> EventM Name State ()
handleEventQuit (VtyEvent (EvKey k ms)) = handleKeyQuit k ms
handleEventQuit (MouseDown (DButton OkButton) BLeft [] _) = doQuit
handleEventQuit (MouseDown (DButton CancelButton) BLeft [] _) = abortQuit
handleEventQuit _ = return ()

handleKeyQuit :: Key -> [Modifier] -> EventM Name State ()
handleKeyQuit KEnter [] = use (sInterface . quitDialog) >>= \case
    Nothing -> return ()
    Just ynd -> case dialogSelection (ynd ^. ynDialog) of
            Just (DButton OkButton, _) -> doQuit
            _ -> abortQuit
handleKeyQuit KEsc [] = abortQuit
handleKeyQuit k ms = handleInQuitDialog (EvKey k ms)

doQuit :: EventM Name State ()
doQuit = doFinalBackup >> halt

abortQuit :: EventM Name State ()
abortQuit = sInterface . quitDialog .= Nothing

handleEventZoomLevel :: ZoomLevel -> BrickEvent Name EventType -> EventM Name State Bool
handleEventZoomLevel _ (MouseDown {}) = return True
handleEventZoomLevel (NormalZoom _) _ = return False
handleEventZoomLevel (RichZoom _) e = handleEventRichZoom e

handleEventRichZoom :: BrickEvent Name EventType -> EventM Name State Bool
handleEventRichZoom (VtyEvent (EvKey k ms)) = handleKeyRichZoom k ms
handleEventRichZoom _ = return False

handleKeyRichZoom :: Key -> [Modifier] -> EventM Name State Bool
handleKeyRichZoom KUp [] = sInterface . richZoom %= fmap richZoomMoveUp >> return True
handleKeyRichZoom KDown [] = sInterface . richZoom %= fmap richZoomMoveDown >> return True
handleKeyRichZoom (KChar '\t') [] = sInterface . richZoom %= fmap richZoomMoveDown >> return True
handleKeyRichZoom KBackTab [] = sInterface . richZoom %= fmap richZoomMoveUp >> return True
handleKeyRichZoom KLeft [] = handleChangeType richZoomNextType
handleKeyRichZoom KRight [] = handleChangeType richZoomPrevType
handleKeyRichZoom (KChar ' ') [] = handleSwitchFormula
handleKeyRichZoom _ _ = return False

handleChangeType :: (RichZoomViewer -> Maybe (RichZoomViewer, FieldType)) -> EventM Name State Bool
handleChangeType f = uses (sInterface . richZoom) (maybe Nothing f) >>= \case
        Just (rz, t) -> do
            sInterface . richZoom .= Just rz
            changeType t
            return True
        Nothing -> return False

handleSwitchFormula :: EventM Name State Bool
handleSwitchFormula = use (sInterface . richZoom) >>= \case
    Just rz -> if rz ^. rzFocus == RzFMark
               then do
                      sInterface . richZoom . _Just . rzIsFormula %= not
                      f <- if rz ^. rzIsFormula
                           then return Nothing
                           else Just . T.concat <$> use (sInterface . activeEditor . _Just . veEditor . to Ed.getEditContents)
                      changeCurrentFieldFormula f
                      return True
               else return False
    Nothing -> return False
changeType :: FieldType -> EventM Name State ()
changeType t = do
                 f <- use sCurrentField
                 sRowStore %= changeFieldType t (fromIntegral f)
                 n <- use sIndex
                 modify $ moveTo n

handleCommonKeys :: BrickEvent Name EventType -> EventM Name State Bool
handleCommonKeys (VtyEvent (EvKey (KChar c) [MCtrl])) = case c of
    'f' -> activateSearch >> return True
    'r' -> toggleInfo >> return True
    't' -> toggleTable >> return True
    'z' -> toggleZoom >> return True
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
handleEventBackLevel (AsTable _) e = handleEventTable e
handleEventBackLevel (AsRows _) e = handleEventRows e

handleEventTable :: BrickEvent Name EventType -> EventM Name State ()
handleEventTable e = handleCommonKeys e >>->> case e of
    VtyEvent (EvKey KUp []) -> backward
    VtyEvent (EvKey KDown []) -> forward
    VtyEvent (EvKey KLeft []) -> fieldBackward
    VtyEvent (EvKey KRight []) -> fieldForward
    MouseDown (ValueColumn c) BLeft [] (Location (_, r)) -> modify $ moveFieldTo c . moveTo r
    MouseDown (ValueColumn _) BScrollUp [] _ -> backward
    MouseDown (ValueColumn _) BScrollDown [] _ -> forward
    _ -> return ()


handleEventRows :: BrickEvent Name EventType -> EventM Name State ()
handleEventRows e = handleCommonKeys e >>->> case e of
    VtyEvent (EvKey KPageUp []) -> backward
    VtyEvent (EvKey KPageDown []) -> forward
    VtyEvent (EvKey KUp []) -> fieldBackward
    VtyEvent (EvKey KDown []) -> fieldForward
    VtyEvent (EvKey KEnter []) -> fieldForward
    VtyEvent (EvKey (KChar 'n') [MCtrl]) -> newRow
    MouseDown FieldNames BLeft [] (Location (_, r)) -> modify $ moveFieldTo r
    MouseDown ValueList BLeft [] (Location (_, r)) -> modify $ moveFieldTo r
    MouseDown (ValueViewer r) BLeft [] _ -> modify (moveFieldTo r) >> handleEdition e
    _ -> handleEdition e

handleEdition :: BrickEvent Name EventType -> EventM Name State ()
handleEdition e = do
    med <- use (sInterface . activeEditor)
    case med of
        Nothing -> return ()
        Just _ -> do
                     B.zoom (sInterface . activeEditor . _Just . veEditor) $ Ed.handleEditorEvent e
                     value <- T.concat <$> use (sInterface . activeEditor . _Just . veEditor . to Ed.getEditContents)
                     use (sInterface . richZoom) >>= \case
                        Just rz -> if rz ^. rzFocus == RzValue
                            then updateCurrentField value
                            else changeCurrentFieldFormula $ Just value
                        _ -> updateCurrentField value

handleInSearchDialog :: Event -> EventM Name State ()
handleInSearchDialog ev = B.zoom (sInterface . searchDialog . _Just . sdDialog) $ handleDialogEvent ev

handleInQuitDialog :: Event -> EventM Name State ()
handleInQuitDialog ev = B.zoom (sInterface . quitDialog . _Just . ynDialog) $ handleDialogEvent ev

