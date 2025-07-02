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
import Brick.Widgets.List (handleListEvent)
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Control.Monad (when)
import Data.Char (isAlpha)
import Data.Text qualified as T
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl), Button (..))
import Model.Expression.Evaluation (evaluate)
import Model.Expression.Parser (parseExpression)
import Model.Expression.RecursionSchemas
import Model.RowStore
import TUI.Base
import TUI.Level
import TUI.State
import Model.Expression.Manipulate (addPositions)

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
handleEventDialogLevel (FieldProperties _) = handleEventFieldProperties

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

handleEventFieldProperties :: BrickEvent Name EventType -> EventM Name State ()
handleEventFieldProperties (VtyEvent (EvKey k ms)) = handleKeyFieldProperties k ms
handleEventFieldProperties _ = return ()

handleKeyFieldProperties :: Key -> [Modifier] -> EventM Name State ()
handleKeyFieldProperties KUp [] = sInterface . fieldProperties %= fmap fieldPropertiesMoveUp
handleKeyFieldProperties KDown [] = sInterface . fieldProperties %= fmap fieldPropertiesMoveDown
handleKeyFieldProperties (KChar '\t') [] = sInterface . fieldProperties %= fmap fieldPropertiesMoveDown
handleKeyFieldProperties KBackTab [] = sInterface . fieldProperties %= fmap fieldPropertiesMoveUp
handleKeyFieldProperties KEsc [] = closeFieldPropertiesDialog
handleKeyFieldProperties KEnter [] = use (sInterface . fieldProperties) >>= \case
    Nothing -> return ()
    Just fp -> case fp ^. fpFocus of
        FpCancel -> closeFieldPropertiesDialog
        _ -> acceptFieldProperties
handleKeyFieldProperties k ms = use (sInterface . fieldProperties) >>= \case
     Nothing -> return ()
     Just si -> case si ^. fpFocus of
         FpType -> case (k, ms) of
                      (KLeft, []) -> zl %= fieldPropertiesPrevType
                      (KRight, []) -> zl %= fieldPropertiesNextType
                      _ -> return ()
         FpOK -> when (k ==  KChar ' ' && null ms) acceptFieldProperties
         FpCancel -> when (k == KChar ' ' && null ms) closeFieldPropertiesDialog
         FpName -> B.zoom (sInterface . activeEditor . _Just . veEditor) $ Ed.handleEditorEvent (VtyEvent (EvKey k ms))
         FpValue -> use (sInterface . activeEditor) >>= \case
                      Nothing -> return ()
                      Just _ -> B.zoom (sInterface . activeEditor . _Just . veEditor) $ Ed.handleEditorEvent (VtyEvent (EvKey k ms))
         FpFMark -> when (k == KChar ' ' && null ms) $ sInterface . fieldProperties . _Just . fpIsFormula %= not
         FpFormula -> handleKeyInFormula k ms
  where zl = sInterface . fieldProperties . _Just

handleKeyInFormula :: Key -> [Modifier] -> EventM Name State ()
handleKeyInFormula k ms = do
    s <- get
    rst <- use sRowStore

    B.zoom (sInterface . fieldProperties . _Just) $ do
       B.zoom (fpFormula . veEditor) $ Ed.handleEditorEvent (VtyEvent (EvKey k ms))
       use fpIsFormula >>= \case
          False -> return ()
          True -> do
                    f <- use (fpFormula . veEditor . to Ed.getEditContents)
                    let ex = parseExpression $ T.concat f
                        r = row (s ^. sIndex) rst
                        v = evaluate r (getDataSources rst) $ addPositions rst ex
                    fpValue .= Right v

closeFieldPropertiesDialog :: EventM Name State ()
closeFieldPropertiesDialog = sInterface . fieldProperties .= Nothing


acceptFieldProperties :: EventM Name State ()
acceptFieldProperties = use (sInterface . fieldProperties) >>= \case
    Nothing -> return ()
    Just fp -> do
        let n = T.concat $ fp ^. fpName . veEditor . to Ed.getEditContents
        changeCurrentFieldName n

        case fp ^. fpValue of
            Left ve -> do
                let v = T.concat $ ve ^. veEditor . to Ed.getEditContents
                updateCurrentField v
            Right _ -> return ()

        changeCurrentFieldType (fp ^. fpType)

        let form = if fp ^. fpIsFormula
                   then Just . T.concat $ fp ^. fpFormula . veEditor . to Ed.getEditContents
                   else Nothing
        changeCurrentFieldFormula form

        closeFieldPropertiesDialog

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

handleCommonKeys :: BrickEvent Name EventType -> EventM Name State Bool
handleCommonKeys (VtyEvent (EvKey (KChar c) [MCtrl])) = case c of
    'f' -> activateSearch >> return True
    'r' -> toggleProperties >> return True
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
handleEdition e = use (sInterface . activeEditor) >>= \case
        Nothing -> return ()
        Just _ -> do
                     B.zoom (sInterface . activeEditor . _Just . veEditor) $ Ed.handleEditorEvent e
                     value <- T.concat <$> use (sInterface . activeEditor . _Just . veEditor . to Ed.getEditContents)
                     updateCurrentField value

handleInSearchDialog :: Event -> EventM Name State ()
handleInSearchDialog ev = B.zoom (sInterface . searchDialog . _Just . sdDialog) $ handleDialogEvent ev

handleInQuitDialog :: Event -> EventM Name State ()
handleInQuitDialog ev = B.zoom (sInterface . quitDialog . _Just . ynDialog) $ handleDialogEvent ev

