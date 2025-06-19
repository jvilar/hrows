{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI (
  startTUI
) where


import Brick hiding (getName, zoom)
import Brick qualified as B
import Brick.BChan qualified as B
import Brick.Widgets.Core qualified as BC
import Brick.Widgets.Dialog
import Brick.Widgets.Edit qualified as Ed
import Brick.Widgets.List hiding (splitAt, reverse)
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (try, SomeException)
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.List(find)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty (imageWidth, imageHeight, translate, Vty (outputIface), supportsMode)
import Graphics.Vty.Config qualified as Vty
import Graphics.Vty.CrossPlatform qualified as Vty
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl))
import Graphics.Vty.Output (Mode(Mouse), setMode)
import HRowsException (HRowsException(..))
import Model.DefaultFileNames (defaultBackupFileName, defaultConfFileName)
import Model.Expression.RecursionSchemas
import Model.Field
import Model.Row (Row)
import Model.RowStore
import Model.SourceInfo
import System.Directory (removeFile)

import TUI.Base
import TUI.Level

maxWidth :: Int
maxWidth = 40

data BackupEvent = BackupEvent deriving (Show)

data State = State { _sRowStore :: RowStore
                   , _sSourceInfo :: Maybe (SourceInfo, [SourceInfo])
                   , _sIndex :: Int
                   , _sCurrentField :: Int
                   , _sInterface :: Interface
                   , _sLog :: [Text]
                   }

makeLenses ''State

logMessage :: Text -> EventM Name State ()
logMessage t = sLog %= reverse . take 10 . (t:) . reverse

initialState :: RowStore -> Maybe (SourceInfo, [SourceInfo]) -> State
initialState rst msi = State { _sRowStore = rst
                             , _sSourceInfo = msi
                             , _sIndex = 0
                             , _sCurrentField = 0
                             , _sInterface = In . Back . AsRows $ mkRowViewer rst 0
                             , _sLog = []
                             }

currentFieldName :: State -> Text
currentFieldName s = fnames (s ^. sRowStore) !! (s ^. sCurrentField)

currentField :: State -> Field
currentField s = row (s ^. sIndex) (s ^. sRowStore) !! (s ^. sCurrentField)

currentFieldType :: State -> Text
currentFieldType s = T.pack $ show $ fieldType (fromIntegral $ s ^. sCurrentField) (s ^. sRowStore)

currentFieldFormula :: State -> Maybe Text
currentFieldFormula s = fieldFormula (fromIntegral $ s ^. sCurrentField) (s ^. sRowStore)

isFormulaCurrentField :: State -> Bool
isFormulaCurrentField s = isFormula (fromIntegral $ s ^. sCurrentField) (s ^. sRowStore)

type EventType = BackupEvent


draw :: State -> [Widget Name]
draw s = bottomRight (txt $ T.unlines $ s ^. sLog) : cata doDraw (s ^. sInterface)
    where
        doDraw (WithDialog dl ws) = renderDialogLevel dl : ws
        doDraw (Zoomed zl ws) = renderZoomLevel zl : ws
        doDraw (Back bl) = [renderBackLevel (windowTitle s) bl]


tshow :: Show a => a -> Text
tshow = T.pack . show


windowTitle :: State -> Text
windowTitle s = T.concat [ getName $ s ^. sRowStore
                         , if changed (s ^. sRowStore) then "*" else ""
                         , " (", tshow $ s ^. sIndex + 1, "/"
                         , tshow $ size $ s ^. sRowStore, ")"
                         ]

bottomRight :: Widget Name -> Widget Name
bottomRight w = Widget Fixed Fixed $ do
    wd <- availWidth <$> getContext
    hg <- availHeight <$> getContext
    r <- render w
    let im = image r
        rw = imageWidth im
        rh = imageHeight im
        hoffset = wd - rw
        voffset = hg - rh
    return r { image = translate hoffset voffset im }

app :: App State EventType Name
app = App { appDraw = draw
          , appChooseCursor = showSelectedCursor
          , appHandleEvent = handleEvent
          , appStartEvent = acceptMouseInput
          , appAttrMap = const myAttrMap
          }

acceptMouseInput :: EventM Name State ()
acceptMouseInput = do
    vty <- getVtyHandle
    let output = outputIface vty
    when (supportsMode output Mouse) $
        liftIO $ setMode output Mouse True

showSelectedCursor :: State ->[CursorLocation Name] -> Maybe (CursorLocation Name)
showSelectedCursor s cs = do
    ed <- s ^. sInterface . activeEditor
    find ((== Just (BC.getName $ ed ^. veEditor)) . cursorLocationName) cs

listKeys :: [Key]
listKeys = [KDown, KUp, KPageUp, KPageDown, KHome, KEnd, KLeft, KRight]

(>>->>) :: EventM Name State Bool -> EventM Name State () -> EventM Name State ()
(>>->>) e1 e2 = e1 >>= \case
                    True -> return ()
                    False -> e2

handleEvent :: BrickEvent Name EventType -> EventM Name State ()
handleEvent (AppEvent BackupEvent) = doBackup
handleEvent e@(MouseDown _ _ _ _) = logMessage $ "Mouse " <> tshow e
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

changeCurrentFieldFormula :: Maybe Formula -> EventM Name State ()
changeCurrentFieldFormula mf = do
    f <- use sCurrentField
    sRowStore %= changeFieldFormula mf (fromIntegral f)
    n <- use sIndex
    modify $ moveTo n


updateCurrentField :: Text -> EventM Name State ()
updateCurrentField t = do
    s <- get
    let (rst, ch) = changeField (s ^. sIndex) (fromIntegral $ s ^. sCurrentField) (toField t) (s ^. sRowStore)
    when (notNull ch) $ do
        sRowStore .= rst
        let r = row (s ^. sIndex) rst
        sInterface %= updateLevels (uRow r (r !! (s ^. sCurrentField)))
  where
    uRow :: Row -> Field -> Level Interface -> Level Interface
    uRow _ _ s@(WithDialog _ _) = s
    uRow _ ft (Zoomed (NormalZoom zv) i) = Zoomed (NormalZoom (over zvValue (updateValueViewer ft) zv)) i
    uRow _ ft (Zoomed (RichZoom rz) i) = Zoomed (RichZoom (over rzValue (updateValueViewer ft) rz)) i
    uRow _ _ a@(Back (AsTable _)) = a
    uRow r _ (Back (AsRows rv)) = Back . AsRows $ updateRvValues r rv

newRow :: EventM Name State ()
newRow = do
    B.zoom sRowStore $ modify addEmptyRow
    s <- uses sRowStore size
    modify $ moveTo (s - 1)

notNull :: [a] -> Bool
notNull [] = False
notNull _ = True


backward :: EventM Name State ()
backward = uses sIndex (subtract 1) >>= modify . moveTo

forward :: EventM Name State ()
forward = uses sIndex (+ 1) >>= modify . moveTo

fieldBackward :: EventM Name State ()
fieldBackward = uses sCurrentField (subtract 1) >>= modify . moveFieldTo

fieldForward :: EventM Name State ()
fieldForward = uses sCurrentField (+ 1) >>= modify . moveFieldTo

toggleZoom :: EventM Name State ()
toggleZoom = use (sInterface . levelZoom) >>= \case
                Just _ -> sInterface . levelZoom .= Nothing
                Nothing -> modify zoom

zoomViewerFromState :: State -> ZoomViewer
zoomViewerFromState s = mkZoomViewer (currentFieldName s)
                                      (isFormulaCurrentField s)
                                      (currentField s)
richZoomViewerFromState :: State -> RichZoomViewer
richZoomViewerFromState s = mkRichZoomViewer (currentFieldName s)
                                                (isFormulaCurrentField s)
                                                (currentField s)
                                                (currentFieldType s)
                                                (currentFieldFormula s)

zoom :: State -> State
zoom s = set (sInterface . levelZoom) (Just . NormalZoom $ zoomViewerFromState s) s

toggleInfo :: EventM Name State ()
toggleInfo = use (sInterface . richZoom) >>= \case
                Just _ -> sInterface . richZoom .= Nothing
                Nothing -> modify info

info :: State -> State
info s = set (sInterface . richZoom) (Just $ mkRichZoomViewer (currentFieldName s)
                                            (isFormulaCurrentField s)
                                            (currentField s)
                                            (currentFieldType s)
                                            (currentFieldFormula s)) s

activateSearch :: EventM Name State ()
activateSearch = do
   index <- use sCurrentField
   tle <- (!! index) <$> uses sRowStore fnames
   vs <- uses sRowStore (fieldValues (fromIntegral index))
   sInterface . searchDialog .= Just (mkSearchDialog SearchList maxWidth tle vs)


deactivateSearch :: EventM Name State ()
deactivateSearch = sInterface . searchDialog .= Nothing


toggleTable :: EventM Name State ()
toggleTable = do
    rst <- use sRowStore
    idx <- use sIndex
    fld <- use sCurrentField
    use (sInterface . tableViewer) >>= \case
                Just _ -> do
                            sInterface . rowViewer .= Just (mkRowViewer rst idx)
                            modify $ moveFieldTo fld
                Nothing -> do
                             sInterface . tableViewer .= Just (buildTable rst idx)
                             modify $ moveTo idx
                             modify $ moveFieldTo fld

moveSearchList :: Event -> EventM Name State ()
moveSearchList e = B.zoom (sInterface . searchDialog . _Just . sdValues) $ handleListEvent e

moveToSelected :: EventM Name State ()
moveToSelected = do
    msd <- use $ sInterface . searchDialog
    let ds = do
               sd <- msd
               dialogSelection (sd ^. sdDialog)
    case ds of
        Just (DButton OkButton, ()) -> do
            deactivateSearch
            let se = do
                       sd <- msd
                       listSelectedElement (sd ^. sdValues)
            case se of
                Nothing -> return ()
                Just (_, t) -> do
                    s <- get
                    let pos = nextPos (fromIntegral $ s ^. sCurrentField) t (s ^. sIndex) (s ^. sRowStore)
                    modify $ moveTo pos
        Just (DButton CancelButton, ()) -> deactivateSearch
        _ -> return ()

handleInSearchDialog :: Event -> EventM Name State ()
handleInSearchDialog ev = B.zoom (sInterface . searchDialog . _Just . sdDialog) $ handleDialogEvent ev

handleInQuitDialog :: Event -> EventM Name State ()
handleInQuitDialog ev = B.zoom (sInterface . quitDialog . _Just . ynDialog) $ handleDialogEvent ev

moveTo :: Int -> State -> State
moveTo pos s
  | valid = over sInterface moveInterface indexUpdated
  | otherwise = s
  where valid = 0 <= pos && pos < size (s ^. sRowStore)
        indexUpdated = set sIndex pos s
        moveInterface = updateLevels mi
        mi se@(WithDialog _ _) = se
        mi (Zoomed (NormalZoom zv) i) = Zoomed (NormalZoom (updateZoomViewer (currentField indexUpdated) zv)) i
        mi (Zoomed (RichZoom rz) i) = Zoomed (RichZoom (updateRichZoomViewer (currentField indexUpdated) rz)) i
        mi (Back (AsTable tv)) = Back $ AsTable (over tvLists (listMoveTo pos) tv)
        mi (Back (AsRows rv)) = let
                           vList = case listSelected (rv ^. rvFieldNames) of
                                     Nothing -> valueList pos (s ^. sRowStore)
                                     Just n -> listMoveTo n (valueList pos (s ^. sRowStore))
                         in Back $ AsRows (set rvValueList vList rv)


moveFieldTo :: Int -> State -> State
moveFieldTo pos s
  | valid = over sInterface moveInterface posUpdated
  | otherwise = s
    where valid = 0 <= pos && pos < length (fnames $ s ^. sRowStore)
          posUpdated = set sCurrentField pos s
          moveInterface = updateLevels mi
          mi (WithDialog dl i) = WithDialog dl i
          mi (Zoomed (NormalZoom _) i) = Zoomed (NormalZoom $ zoomViewerFromState posUpdated) i
          mi (Zoomed (RichZoom _) i) = Zoomed (RichZoom $ richZoomViewerFromState posUpdated) i
          mi (Back (AsTable tv)) = Back $ AsTable (set tvCurrentField pos tv)
          mi (Back (AsRows rv)) = Back $ AsRows (over rvFieldNames (listMoveTo pos) $
                                        over rvValueList (listMoveTo pos) rv)

doSave :: EventM Name State ()
doSave = do
    msi <- use sSourceInfo
    case msi of
        Nothing -> logMessage "No source info available"
        Just (si, sis) -> do
            rst <- use sRowStore
            let pc = siPathAndConf si
                si' = case confPath pc of
                         Nothing -> let
                                       cnf = defaultConfFileName $ path pc
                                    in changePathAndConf (pc { confPath = Just cnf }) si
                         Just _ -> si
            liftIO $ writeRowStore si' sis rst
            sRowStore %= setUnchanged

backupLoop :: B.BChan EventType -> IO ()
backupLoop chan = do
  threadDelay $ 60 * 1000000
  B.writeBChan chan BackupEvent
  backupLoop chan

doBackup :: EventM Name State ()
doBackup = use sRowStore >>= (\case
    False -> return ()
    True -> do
        msi <- use sSourceInfo
        case msi of
            Nothing -> return ()
            Just (si, sis) -> do
                let conf = defaultBackupFileName <$> confPath (siPathAndConf si)
                    fp = defaultBackupFileName $ path (siPathAndConf si)
                    si' = changePathAndConf (PathAndConf fp conf) si
                rst <- use sRowStore
                (liftIO . try $ writeRowStore si' sis rst) >>= \case
                    Right _ -> return ()
                    Left (HRowsException e) -> logMessage ("Backup error: " <> e))
        . changed

doFinalBackup :: EventM Name State ()
doFinalBackup = use sRowStore >>= (\case
    True -> doBackup
    False -> do
        msi <- use sSourceInfo
        case msi of
            Nothing -> return ()
            Just (si, _) -> do
                let conf = defaultBackupFileName <$> confPath (siPathAndConf si)
                    fp = defaultBackupFileName $ path (siPathAndConf si)
                void . liftIO $ ((try $ do
                                    removeFile fp
                                    maybe (return ()) removeFile conf) :: IO (Either SomeException ())))
        . changed

startTUI :: RowStore -> Maybe (SourceInfo, [SourceInfo]) -> IO ()
startTUI rst msi = do
  eventChan <- B.newBChan 10
  _ <- forkIO $ backupLoop eventChan
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty (Just eventChan) app (initialState rst msi)
  return ()
