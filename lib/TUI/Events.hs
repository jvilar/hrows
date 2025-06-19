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
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl))
import Model.Expression.RecursionSchemas
import Model.Field
import Model.RowStore

import TUI.Base
import TUI.Level
import TUI.State

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
handleEvent e@(MouseDown _ _ _ _) = logMessage $ "Mouse " <> T.pack (show e)
handleEvent e = handleGlobalEvent e
   >>->> (use sInterface >>= handleInLevel e . out)

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
handleEventSearch _ = return ()

handleKeySearch :: Key -> [Modifier] -> EventM Name State ()
handleKeySearch k [] | k `elem` listKeys = moveSearchList (EvKey k [])
handleKeySearch KEnter [] = moveToSelected
handleKeySearch KEsc [] = deactivateSearch
handleKeySearch k ms = handleInSearchDialog (EvKey k ms)

handleEventQuit :: BrickEvent Name EventType -> EventM Name State ()
handleEventQuit (VtyEvent (EvKey k ms)) = handleKeyQuit k ms
handleEventQuit _ = return ()

handleKeyQuit :: Key -> [Modifier] -> EventM Name State ()
handleKeyQuit KEnter [] = use (sInterface . quitDialog) >>= \case
    Nothing -> return ()
    Just ynd -> case dialogSelection (ynd ^. ynDialog) of
            Just (DButton OkButton, _) -> doFinalBackup >> halt
            _ -> sInterface . quitDialog .= Nothing
handleKeyQuit KEsc [] = sInterface . quitDialog .= Nothing
handleKeyQuit k ms = handleInQuitDialog (EvKey k ms)

handleEventZoomLevel :: ZoomLevel -> BrickEvent Name EventType -> EventM Name State Bool
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
    _ -> return ()


handleEventRows :: BrickEvent Name EventType -> EventM Name State ()
handleEventRows e = handleCommonKeys e >>->> case e of
    VtyEvent (EvKey KPageUp []) -> backward
    VtyEvent (EvKey KPageDown []) -> forward
    VtyEvent (EvKey KUp []) -> fieldBackward
    VtyEvent (EvKey KDown []) -> fieldForward
    VtyEvent (EvKey KEnter []) -> fieldForward
    VtyEvent (EvKey (KChar 'n') [MCtrl]) -> newRow
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

