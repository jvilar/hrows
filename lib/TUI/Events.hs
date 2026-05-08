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
import Brick.Widgets.Edit qualified as Ed
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Control.Monad (when, unless)
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
    rst <- use sRowStore
    if changed rst
    then sInterface . quitDialog .= Just (mkYesNoDialog "Are you sure you want to quit?" "Quit")
    else doQuit
    return True
handleGlobalEvent (VtyEvent (EvKey (KChar 'w') [MCtrl])) = doSave >> return True
handleGlobalEvent _ = return False

handleEventDialogLevel :: DialogLevel -> BrickEvent Name EventType -> EventM Name State ()
handleEventDialogLevel (Searching _) ev = do
    res <- B.zoom (sInterface . searchDialog . anon emptySearchDialog (const False)) $ handleEventSearchDialog ev
    case res of
        DoNothing -> return ()
        DialogCancel -> deactivateSearch
        DialogResult t -> searchSelection t
handleEventDialogLevel (Quitting _) ev = do
    res <- B.zoom (sInterface . quitDialog . anon emptyYesNoDialog (const False)) $ handleEventYesNoDialog ev
    case res of
        DoNothing -> return ()
        DialogCancel -> abortQuit
        DialogResult () -> doQuit
handleEventDialogLevel (FieldProperties _) ev = handleEventFieldProperties ev

searchSelection :: T.Text -> EventM Name State ()
searchSelection t = do
    deactivateSearch
    s <- get
    let pos = nextPos (fromIntegral $ s ^. sCurrentField) t (s ^. sIndex) (s ^. sRowStore)
    modify $ moveTo pos

handleEventFieldProperties :: BrickEvent Name EventType -> EventM Name State ()
handleEventFieldProperties (VtyEvent (EvKey k ms)) = handleKeyFieldProperties k ms
handleEventFieldProperties (MouseDown n BLeft [] _) = case n of
    DButton OkButton -> acceptFieldProperties
    DButton CancelButton -> closeFieldPropertiesDialog
    FieldPropertiesNameEditor -> focus .= FpName
    FieldPropertiesValueEditor -> focus .= FpValue
    FieldPropertiesTypeSelector -> focus .= FpType
    FieldPropertiesFormulaMark -> do
                                    focus .= FpFMark
                                    switchIsFormula
    FieldPropertiesFormulaEditor -> focus .= FpFormula
    _ -> return ()
  where focus = sInterface . fieldProperties . _Just . fpFocus
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
                      Just _ -> B.zoom (sInterface . activeEditor . _Just) $ handleEventValueEditor (VtyEvent (EvKey k ms))
         FpFMark -> when (k == KChar ' ' && null ms) switchIsFormula
         FpFormula -> handleKeyInFormula k ms
  where zl = sInterface . fieldProperties . _Just

switchIsFormula :: EventM Name State ()
switchIsFormula = B.zoom (sInterface . fieldProperties . _Just) $ do
    f <- use $ fpValue . vvValue
    use fpIsFormula >>= \case
        True -> do
                  fpIsFormula .= False
                  fpValue .= Left (mkEditor FieldPropertiesValueEditor f)
        False -> do
                   fpIsFormula .= True
                   fpValue .= Right f

handleKeyInFormula :: Key -> [Modifier] -> EventM Name State ()
handleKeyInFormula k ms = do
    s <- get
    rst <- use sRowStore

    B.zoom (sInterface . fieldProperties . _Just) $ do
       B.zoom (fpFormula . veEditor) $ Ed.handleEditorEvent (VtyEvent (EvKey k ms))
       use fpIsFormula >>= \case
          False -> return ()
          True -> do
                    f <- use $ fpFormula . veContent
                    t <- use fpType
                    let ex = parseExpression f
                        r = row (s ^. sIndex) rst
                        v = convert t [evaluate r (getDataSources rst) $ addPositions rst ex]
                    fpValue .= Right v

closeFieldPropertiesDialog :: EventM Name State ()
closeFieldPropertiesDialog = sInterface . fieldProperties .= Nothing


acceptFieldProperties :: EventM Name State ()
acceptFieldProperties = use (sInterface . fieldProperties) >>= \case
    Nothing -> return ()
    Just fp -> do
        let n = fp ^. fpName . veContent
        changeCurrentFieldName n

        case fp ^. fpValue of
            Left ve -> updateCurrentField $ ve ^. veField
            Right _ -> return ()

        changeCurrentFieldType (fp ^. fpType)

        let form = if fp ^. fpIsFormula
                   then Just $ fp ^. fpFormula . veContent
                   else Nothing
        changeCurrentFieldFormula form

        closeFieldPropertiesDialog

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
    B.zoom veEditor $ Ed.handleEditorEvent ev
    ve <- get
    let f = convertKeepText (ve ^. veType) (toField $ ve ^. veContent)
    modify $ updateEditor f

