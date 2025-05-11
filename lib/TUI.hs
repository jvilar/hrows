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
import Brick.BChan qualified as B
import Brick qualified as B
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core qualified as BC
import Brick.Widgets.Dialog
import Brick.Widgets.Edit qualified as Ed
import Brick.Widgets.List hiding (splitAt, reverse)
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Data.List(transpose, intersperse, find)
import Data.Maybe(fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as Tz
import Data.Vector qualified as V
import Graphics.Vty.Attributes (defAttr, bold, reverseVideo, withStyle, withBackColor, withForeColor, black, rgbColor)
import Graphics.Vty.Config qualified as Vty
import Graphics.Vty.CrossPlatform qualified as Vty
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl))

import Model.Field
import Model.Expression.RecursionSchemas
import Model.RowStore
import Model.SourceInfo
import Graphics.Vty (imageWidth, imageHeight, translate)
import Control.Monad (when, void)
import Model.Row (Row)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.IO.Class (liftIO)
import HRowsException (HRowsException(..))
import Model.DefaultFileNames (defaultBackupFileName)
import Control.Exception (try, SomeException)
import System.Directory (removeFile)
import Data.BitVector (msb)

maxWidth :: Int
maxWidth = 40

data BackupEvent = BackupEvent

data Name = DButton DialogButton
          | FieldNames
          | SearchList
          | RichZoomEditor
          | ValueColumn Int
          | ValueViewer Int
          | ValueList
          | ZoomEditor
          deriving (Eq, Ord, Show)

data DialogButton = OkButton | CancelButton deriving (Eq, Ord, Show)

data Level i = Searching SearchDialog i
             | Zoomed ZoomViewer i
             | RichZoomed RichZoomViewer i
             | AsTable TableViewer
             | AsRows RowViewer deriving Functor

isSearching :: Level i -> Bool
isSearching (Searching _ _) = True
isSearching _ = False

isZoomed :: Level i -> Bool
isZoomed (Zoomed _ _) = True
isZoomed _ = False

isRichZoomed :: Level i -> Bool
isRichZoomed (RichZoomed _ _) = True
isRichZoomed _ = False

isAsTable :: Level i -> Bool
isAsTable (AsTable _) = True
isAsTable _ = False

isAsRows :: Level i -> Bool
isAsRows (AsRows _) = True
isAsRows _ = False

type Interface = Fix Level

updateLevels :: (Level Interface -> Level Interface) -> Interface -> Interface
updateLevels = bottomUp

data State = State { _sRowStore :: RowStore
                   , _sSourceInfo :: Maybe (SourceInfo, [SourceInfo])
                   , _sIndex :: Int
                   , _sCurrentField :: Int
                   , _sInterface :: Interface
                   , _sLog :: [Text]
                   }

data SearchDialog = SearchDialog { _sdValues :: List Name Text
                                 , _sdDialog :: Dialog () Name
                                 }

data ValueEditor = ValueEditor { _veEditor :: Ed.Editor Text Name
                               , _veIsError :: Bool
                               }

type ValueViewer = Either ValueEditor Field

data RowViewer = RowViewer { _rvFieldNames :: List Name Text
                           , _rvFieldWidth :: Int
                           , _rvValueList :: List Name ValueViewer
                           }

data TableViewer = TableViewer { _tvFieldNames :: [Text]
                               , _tvColWidths :: [Int]
                               , _tvColumns :: [List Name Text]
                               , _tvCurrentField :: Int
                               }

data ZoomViewer = ZoomViewer { _zvTitle :: Text
                             , _zvValue :: ValueViewer
                             }

data RichZoomViewer = RichZoomViewer { _ivTitle :: Text
                                     , _ivValue :: ValueViewer
                                     , _ivType :: Text
                                     , _ivFormula :: Maybe Text
                                     , _ivFocus :: Int -- 0: title, 1: type, 2: formula
                                     }

tvLists :: Traversal' TableViewer (List Name Text)
tvLists f (TableViewer fl cw cs cf) = TableViewer fl cw <$> traverse f cs <*> pure cf

makeLenses ''RowViewer
makeLenses ''SearchDialog
makeLenses ''State
makeLenses ''TableViewer
makeLenses ''ValueEditor
makeLenses ''ZoomViewer
makeLenses ''RichZoomViewer

updateRvValues :: [Field] -> RowViewer -> RowViewer
updateRvValues ts rv = over rvValueList (listReplace v i) rv
  where i = listSelected $ rv ^. rvValueList
        l = listElements (rv ^. rvValueList)
        v = V.fromList $ zipWith updateValueViewer ts (V.toList l)

getLevel :: (forall i . Level i -> Bool) -> Interface -> Maybe Interface
getLevel f = para search
    where search :: Level (Interface, Maybe Interface) -> Maybe Interface
          search l@(Searching sd (i, ms)) = if f l then Just (In $ Searching sd i) else ms
          search l@(Zoomed zv (i, ms)) = if f l then Just (In $ Zoomed zv i) else Nothing
          search l@(RichZoomed iv (i, ms)) = if f l then Just (In $ RichZoomed iv i) else ms
          search l@(AsTable tv) = if f l then Just (In $ AsTable tv) else Nothing
          search l@(AsRows rv) = if f l then Just (In $ AsRows rv) else Nothing

removeLevel :: (forall i . Level i -> Bool) -> Interface -> Interface
removeLevel f = updateLevels remL
    where remL l@(Searching _ i) = if f l then out i else l
          remL l@(Zoomed _ i) = if f l then out i else l
          remL l@(RichZoomed _ i) = if f l then out i else l
          remL l@(AsTable _) = if f l then error "Cannot remove table" else l
          remL l@(AsRows _) = if f l then error "Cannot remove row viewer" else l

levelSearch :: Lens' Interface (Maybe SearchDialog)
levelSearch = lens getter setter
    where getter i = case getLevel isSearching i of
                         Just (In (Searching sd _)) -> Just sd
                         _ -> Nothing

          setter i Nothing = removeLevel isSearching i
          setter i (Just sd) = updateLevels (addD sd) i
          addD :: SearchDialog -> Level Interface -> Level Interface
          addD _ (Searching _ i) = out i
          addD sd (Zoomed zv (In (Searching _ i))) = Searching sd (In $ Zoomed zv i)
          addD _ z@(Zoomed _ _) = z
          addD sd (RichZoomed iv (In (Searching _ i))) = Searching sd (In $ RichZoomed iv i)
          addD _ z@(RichZoomed _ _) = z
          addD sd t@(AsTable _) = Searching sd (In t)
          addD sd r@(AsRows _) = Searching sd (In r)

levelZoom :: Lens' Interface (Maybe ZoomViewer)
levelZoom = lens getter setter
    where getter i = case getLevel isZoomed i of
                         Just (In (Zoomed zv _)) -> Just zv
                         _ -> Nothing

          setter i Nothing = removeLevel isZoomed i
          setter i (Just z) = updateLevels (addZ z) i
          addZ :: ZoomViewer -> Level Interface -> Level Interface
          addZ _ s@(Searching _ _) = s
          addZ _ (Zoomed _ i) = out i
          addZ _ (RichZoomed _ i) = out i
          addZ z t@(AsTable _) = Zoomed z (In t)
          addZ z r@(AsRows _) = Zoomed z (In r)

levelRichZoom :: Lens' Interface (Maybe RichZoomViewer)
levelRichZoom = lens getter setter
    where getter i = case getLevel isRichZoomed i of
                         Just (In (RichZoomed iv _)) -> Just iv
                         _ -> Nothing

          setter i Nothing = removeLevel isRichZoomed i
          setter i (Just iv) = updateLevels (addI iv) i
          addI :: RichZoomViewer -> Level Interface -> Level Interface
          addI _ s@(Searching _ _) = s
          addI _ (Zoomed _ i) = out i
          addI _ (RichZoomed _ i) = out i
          addI iv t@(AsTable _) = RichZoomed iv (In t)
          addI iv r@(AsRows _) = RichZoomed iv (In r)

levelTable :: Lens' Interface (Maybe TableViewer)
levelTable = lens getter setter
    where getter i = case getLevel isAsTable i of
                         Just (In (AsTable tv)) -> Just tv
                         _ -> Nothing

          setter _ Nothing = error "Cannot remove table: add a row viewer"
          setter i (Just tv) = updateLevels (addT tv) i
          addT :: TableViewer -> Level Interface -> Level Interface
          addT _ s@(Searching _ _) = s
          addT _ z@(Zoomed _ _) = z
          addT _ i@(RichZoomed _ _) = i
          addT tv (AsRows _) = AsTable tv
          addT tv (AsTable _) = AsTable tv

levelRows :: Lens' Interface (Maybe RowViewer)
levelRows = lens getter setter
    where getter i = case getLevel isAsRows i of
                         Just (In (AsRows rv)) -> Just rv
                         _ -> Nothing

          setter _ Nothing = error "Cannot remove row viewer: add a table"
          setter i (Just rv) = updateLevels (addR rv) i
          addR :: RowViewer -> Level Interface -> Level Interface
          addR _ s@(Searching _ _) = s
          addR _ z@(Zoomed _ _) = z
          addR _ i@(RichZoomed _ _) = i
          addR rv (AsTable _) = AsRows rv
          addR rv (AsRows _) = AsRows rv

activeEditor :: Lens' Interface (Maybe ValueEditor)
activeEditor = lens getter setter
    where getter = cata ae
          ae (Searching _ _) = Nothing
          ae (Zoomed z _) = case z ^. zvValue of
                              Left ve -> Just ve
                              _ -> Nothing
          ae (RichZoomed iv _) = case iv ^. ivValue of
                                   Left ve -> Just ve
                                   _ -> Nothing
          ae (AsTable _) = Nothing
          ae (AsRows rv) = case listSelectedElement (rv ^. rvValueList) of
                             Just (_, Left ve) -> Just ve
                             _ -> Nothing

          setter _ Nothing = error "Cannot remove editor"
          setter i (Just ve) = para (addE ve) i
          addE :: ValueEditor -> Level (Interface, Interface) -> Interface
          addE _ (Searching sd (_, i)) = In $ Searching sd i
          addE ve (Zoomed zv (i, _)) = In $ Zoomed (set zvValue (Left ve) zv) i
          addE ve (RichZoomed iv (i, _)) = In $ RichZoomed (set ivValue (Left ve) iv) i
          addE _ (AsTable _) = error "Cannot add editor to table"
          addE ve (AsRows rv) = In $ AsRows (set (rvValueList . element (fromMaybe 0 $ listSelected $ rv ^. rvValueList)) (Left ve) rv)


logMessage :: Text -> EventM Name State ()
logMessage t = sLog %= reverse . take 10 . (t:) . reverse

mkSearchDialog :: Name -> Int -> Text -> [Text] -> SearchDialog
mkSearchDialog n w ttle values = SearchDialog (list n (V.fromList values) 1)
                                            (dialog (Just $ txt ttle)
                                                    (Just (DButton OkButton, [ ("OK", DButton OkButton, ())
                                                              , ("Cancel", DButton CancelButton, ())]))
                                                    w
                                            )


renderSearchDialog :: SearchDialog -> Widget Name
renderSearchDialog sd = renderDialog (sd ^. sdDialog) $
                           vLimit (V.length $ listElements $ sd ^. sdValues)
                                  (renderList renderValue False $ sd ^. sdValues)

renderZoomViewer :: ZoomViewer -> Widget Name
renderZoomViewer zv = centerLayer $ joinBorders $
                       hLimitPercent 95 $
                       borderWithLabel (withAttr titleAttr $ txt $ zv ^. zvTitle) $
                         renderValueViewer (zv ^. zvValue)
                         <=>
                         hBorder
                         <=>
                         hCenter (txt "C-r: rich zoom, C-z: close zoom")

renderRichZoomViewer :: RichZoomViewer -> Widget Name
renderRichZoomViewer iv = centerLayer $ joinBorders $
                       hLimitPercent 95 $
                       borderWithLabel (withAttr titleAttr $ txt $ iv ^. ivTitle) $
                         renderValueViewer (iv ^. ivValue)
                         <=>
                         hBorder
                         <=>
                         (if iv ^. ivFocus == 1
                          then withAttr selectedElementAttr
                          else id) (txt $ iv ^. ivType)
                         <=>
                         case iv ^. ivFormula of
                           Nothing -> emptyWidget
                           Just f -> hBorder
                                     <=>
                                     txt f
                         <=>
                         hBorder
                         <=>
                         hCenter (txt "C-r: close rich zoom, C-z: zoom")

renderValueViewer :: ValueViewer -> Widget Name
renderValueViewer = either renderValueEditor renderField
  where renderField f = let
              attr = if isError f then errorAttr else formulaAttr
           in withAttr attr . myTxt $ toString f

initialState :: RowStore -> Maybe (SourceInfo, [SourceInfo]) -> State
initialState rst msi = State { _sRowStore = rst
                             , _sSourceInfo = msi
                             , _sIndex = 0
                             , _sCurrentField = 0
                             , _sInterface = In . AsRows $ mkRowViewer rst 0
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

mkRowViewer :: RowStore -> Int -> RowViewer
mkRowViewer rst pos = RowViewer { _rvFieldNames = listMoveTo pos fl
                                , _rvFieldWidth = min maxWidth (V.maximum . V.map T.length $ listElements fl)
                                , _rvValueList = valueList pos rst
                                }
                            where fl = fieldList rst

mkZoomViewer :: State -> ZoomViewer
mkZoomViewer s = ZoomViewer (currentFieldName s) $ if isFormulaCurrentField s
                                                   then Right (currentField s)
                                                   else Left (mkEditor ZoomEditor $ currentField s)

mkRichZoomViewer :: State -> RichZoomViewer
mkRichZoomViewer s = RichZoomViewer (currentFieldName s)
                            (if isFormulaCurrentField s
                             then Right (currentField s)
                             else Left (mkEditor RichZoomEditor $ currentField s)
                             )
                            (currentFieldType s)
                            (currentFieldFormula s)
                            0

updateZoomViewer :: Field -> ZoomViewer -> ZoomViewer
updateZoomViewer f = over zvValue (either (Left . updateEditor f) (Right . const f))

updateRichZoomViewer :: Field -> RichZoomViewer -> RichZoomViewer
updateRichZoomViewer f = over ivValue (either (Left . updateEditor f) (Right . const f))

richZoomMoveUp :: RichZoomViewer -> RichZoomViewer
richZoomMoveUp = over ivFocus (\i -> max 0 (i - 1))

richZoomMoveDown :: RichZoomViewer -> RichZoomViewer
richZoomMoveDown = over ivFocus (\i -> min 2 (i + 1))

richZoomNextType :: RichZoomViewer -> Maybe (RichZoomViewer, FieldType)
richZoomNextType rz
   | rz ^. ivFocus /= 1 || t == maxBound || t' == TypeEmpty = Nothing
   | otherwise = Just (set ivType (T.pack $ show t') rz, t')
   where t = read . T.unpack $ rz ^. ivType
         t' = succ t

richZoomPrevType :: RichZoomViewer -> Maybe (RichZoomViewer, FieldType)
richZoomPrevType rz
   | rz ^. ivFocus /= 1 || t == minBound = Nothing
   | otherwise = Just (set ivType (T.pack $ show t') rz, t')
   where t = read . T.unpack $ rz ^. ivType
         t' = pred t

mkEditor :: Name -> Field -> ValueEditor
mkEditor n f = ValueEditor (Ed.editor n (Just 1) $ toString f) (isError f)

fieldList :: RowStore -> List Name Text
fieldList rst = list FieldNames (V.fromList $ fnames rst) 1

valueList :: Int -> RowStore -> List Name ValueViewer
valueList pos rst = list ValueList (V.fromList $ zipWith valueWidget [0..] (row pos rst)) 1
    where valueWidget n f
              | isFormula n rst = Right f
              | otherwise = Left $ mkEditor (ValueViewer $ fromIntegral n) f

buildTable :: RowStore -> Int -> TableViewer
buildTable rst = TableViewer ns ws cls
    where cs = transpose $ map (map toString) $ rows rst
          cls = zipWith f [0..] cs
          f n r = list (ValueColumn n) (V.fromList r) 1
          ns = fnames rst
          ws = zipWith max (map T.length ns)
                           $ map (maximum . map T.length) cs


type EventType = BackupEvent


draw :: State -> [Widget Name]
draw s = bottomRight (txt $ T.unlines $ s ^. sLog) : cata doDraw (s ^. sInterface)
    where
        doDraw (Searching sd ws) = renderSearchDialog sd : ws
        doDraw (Zoomed zv ws) = renderZoomViewer zv : ws
        doDraw (RichZoomed iv ws) = renderRichZoomViewer iv : ws
        doDraw (AsTable tv) = [renderBack (windowTitle s) (renderTableViewer tv) tableHelp]
        doDraw (AsRows rv) = [renderBack (windowTitle s) (renderRowViewer rv) rowHelp]
        tableHelp = "C-z: zoom, C-r: rich zoom, C-t: return to field view, C-f: find, C-w: write, C-q: exit"
        rowHelp = "C-z: zoom, C-r: rich zoom, C-t: table view, C-f: find, C-n: new row, C-w: write, C-q: exit"


renderBack :: Text -> Widget Name -> Text -> Widget Name
renderBack t content help = joinBorders $ center $
       borderWithLabel (withAttr titleAttr . txt $ t) $
       content
       <=>
       hBorder
       <=>
       hCenter (txt help)


tshow :: Show a => a -> Text
tshow = T.pack . show


windowTitle :: State -> Text
windowTitle s = T.concat [ getName $ s ^. sRowStore
                         , if changed (s ^. sRowStore) then "*" else ""
                         , " (", tshow $ s ^. sIndex + 1, "/"
                         , tshow $ size $ s ^. sRowStore, ")"
                         ]

renderRowViewer :: RowViewer -> Widget Name
renderRowViewer rv = Widget Greedy Fixed $ do
    h <- availHeight <$> getContext
    let v = min (V.length $ listElements $ rv ^. rvFieldNames) (h-2)
    render $ vLimit v $ hBox [
                hLimit (rv ^. rvFieldWidth) (renderList renderName False $ rv ^. rvFieldNames)
                , vBorder
                , renderList (const renderValueViewer) False
                      $ rv ^. rvValueList
              ]

renderTableViewer :: TableViewer -> Widget Name
renderTableViewer tv = Widget Greedy Fixed $ do
    h <- availHeight <$> getContext
    w <- availWidth <$> getContext
    let v = min (V.length $ listElements $ head $ tv ^. tvColumns) (h-4)
        ws = allocateWidths (tv ^. tvCurrentField) w $ tv ^. tvColWidths
    render ( vLimit 1 (hBox $ withWidths (\(i, t) ->
                                              if i == tv ^. tvCurrentField
                                              then withAttr selectedElementAttr $ myTxt t
                                              else withAttr titleAttr $ myTxt t)
                               ws
                               (zip [0..] $ tv ^. tvFieldNames)
                      )
             <=>
             hBorder
             <=>
             vLimit v ( hBox $ withWidths (renderList renderValue False)
                               ws
                               (tv ^. tvColumns)
                      )
           )


allocateWidths :: Int -> Int -> [Int] -> [Int]
allocateWidths curF w ws
  | w < s = let
               (lft, wcurF:rgt) = splitAt curF ws1
               wCurrent = min w (wcurF - 1)
               wsLeft= reverse . upto (w - wCurrent) $ reverse lft
               wRest = w - wCurrent - sum wsLeft
            in map (max 0 . subtract 1) $ wsLeft ++ [wCurrent + 1] ++ upto wRest rgt
  | otherwise = over (taking n traversed) (+ (dw+1))
              $ over (dropping n traversed) (+ dw) ws
  where ws1 = map succ ws
        s = sum ws1
        l = length ws1
        (dw, n) = (w - s) `divMod` l
        upto _ [] = []
        upto ml (x:xs) = v:upto (ml-v) xs
            where v = min ml x

withWidths :: (a -> Widget Name) -> [Int] -> [a] -> [Widget Name]
withWidths f ws l = intersperse vBorder
                  $ map (uncurry $ withWidth f)
                  $ filter ((>0) . fst)
                  $ zip ws l

withWidth :: (a -> Widget Name) -> Int -> a -> Widget Name
withWidth f w = hLimit w . padRight Max . f

renderName :: Bool -> Text -> Widget Name
renderName = (withAttr titleAttr .) . renderElement


renderValue :: Bool -> Text -> Widget Name
renderValue = renderElement

renderValueEditor :: ValueEditor -> Widget Name
renderValueEditor ve = Ed.renderEditor ((if ve ^. veIsError
                        then withAttr errorAttr
                        else id) . (txt . T.unlines)) True $ ve ^. veEditor

renderElement :: Bool -> Text -> Widget Name
renderElement False = myTxt
renderElement True = withAttr selectedElementAttr . myTxt


myTxt :: Text -> Widget n
myTxt t = Widget Fixed Fixed $ do
      w <- availWidth <$> getContext
      let l = T.length t
          t' | l <= w = T.justifyLeft w ' ' t
             | otherwise = T.take (w-3) t <> "..."
      render $ txt t'

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
          , appStartEvent = return ()
          , appAttrMap = const myAttrMap
          }

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
handleEvent e = handleGlobalEvent e
   >>->> (use sInterface >>= handleInLevel e . out)

handleInLevel :: BrickEvent Name EventType -> Level Interface -> EventM Name State ()
handleInLevel e (Searching _ _) = handleEventSearch e
handleInLevel e (Zoomed _ i) = handleInLevel e $ out i
handleInLevel e (RichZoomed _ i) = handleEventRichZoom e >>->> handleInLevel e (out i)
handleInLevel e (AsTable _) = handleEventTable e
handleInLevel e (AsRows _) = handleEventRows e

handleGlobalEvent :: BrickEvent Name EventType -> EventM Name State Bool
handleGlobalEvent (VtyEvent (EvKey (KChar 'q') [MCtrl])) = doFinalBackup >> halt >> return True
handleGlobalEvent (VtyEvent (EvKey (KChar 'w') [MCtrl])) = doSave >> return True
handleGlobalEvent _ = return False

handleEventSearch :: BrickEvent Name EventType -> EventM Name State ()
handleEventSearch (VtyEvent (EvKey k ms)) = handleKeySearch k ms
handleEventSearch _ = return ()

handleKeySearch :: Key -> [Modifier] -> EventM Name State ()
handleKeySearch k [] | k `elem` listKeys = moveSearchList (EvKey k [])
handleKeySearch KEnter [] = moveToSelected
handleKeySearch KEsc [] = deactivateSearch
handleKeySearch k ms = handleInSearchDialog (EvKey k ms)

handleEventRichZoom :: BrickEvent Name EventType -> EventM Name State Bool
handleEventRichZoom (VtyEvent (EvKey k ms)) = handleKeyRichZoom k ms
handleEventRichZoom _ = return False

handleKeyRichZoom :: Key -> [Modifier] -> EventM Name State Bool
handleKeyRichZoom KUp [] = sInterface . levelRichZoom %= fmap richZoomMoveUp >> return True
handleKeyRichZoom KDown [] = sInterface . levelRichZoom %= fmap richZoomMoveDown >> return True
handleKeyRichZoom (KChar '\t') [] = sInterface . levelRichZoom %= fmap richZoomMoveDown >> return True
handleKeyRichZoom KBackTab [] = sInterface . levelRichZoom %= fmap richZoomMoveUp >> return True
handleKeyRichZoom KLeft [] = do
    uses (sInterface . levelRichZoom) (maybe Nothing richZoomNextType) >>= \case
        Just (rz, t) -> do
            sInterface . levelRichZoom .= Just rz
            changeType t
            return True
        Nothing -> return True
handleKeyRichZoom KRight [] = do
    uses (sInterface . levelRichZoom) (maybe Nothing richZoomPrevType) >>= \case
        Just (rz, t) -> do
            sInterface . levelRichZoom .= Just rz
            changeType t
            return True
        Nothing -> return True
handleKeyRichZoom _ _ = return False

changeType :: FieldType -> EventM Name State ()
changeType t = do
                 f <- use sCurrentField
                 sRowStore %= changeFieldType t (fromIntegral f)
                 n <- use sIndex
                 modify $ moveTo n

showFocus = use (sInterface . levelRichZoom) >>= logMessage . T.pack . show . fmap (^. ivFocus)

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
                     value <- use (sInterface . activeEditor . _Just . veEditor . to Ed.getEditContents)
                     updateCurrentField $ T.concat value

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
    uRow _ _ s@(Searching _ _) = s
    uRow _ ft (Zoomed zv i) = Zoomed (over zvValue (updateValueViewer ft) zv) i
    uRow _ ft (RichZoomed iv i) = RichZoomed (over ivValue (updateValueViewer ft) iv) i
    uRow _ _ a@(AsTable _) = a
    uRow r _ (AsRows rv) = AsRows $ updateRvValues r rv

newRow :: EventM Name State ()
newRow = do
    B.zoom sRowStore $ modify addEmptyRow
    s <- uses sRowStore size
    modify $ moveTo (s - 1)

notNull :: [a] -> Bool
notNull [] = False
notNull _ = True

updateValueViewer :: Field -> ValueViewer -> ValueViewer
updateValueViewer f = either (Left . updateEditor f) (const $ Right f)

updateEditor :: Field -> ValueEditor -> ValueEditor
updateEditor f ve | t == T.concat (Ed.getEditContents $ ve ^. veEditor) = set veIsError (isError f) ve
                  | otherwise = over veEditor (Ed.applyEdit (const $ Tz.textZipper [t] $ Just 1))
                                $ set veIsError (isError f) ve
                  where t = toString f

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

zoom :: State -> State
zoom s = set (sInterface . levelZoom) (Just $ mkZoomViewer s) s

toggleInfo :: EventM Name State ()
toggleInfo = use (sInterface . levelRichZoom) >>= \case
                Just _ -> sInterface . levelRichZoom .= Nothing
                Nothing -> modify info

info :: State -> State
info s = set (sInterface . levelRichZoom) (Just $ mkRichZoomViewer s) s

activateSearch :: EventM Name State ()
activateSearch = do
   index <- use sCurrentField
   tle <- (!! index) <$> uses sRowStore fnames
   vs <- uses sRowStore (fieldValues (fromIntegral index))
   sInterface . levelSearch .= Just (mkSearchDialog SearchList maxWidth tle vs)


deactivateSearch :: EventM Name State ()
deactivateSearch = sInterface . levelSearch .= Nothing


toggleTable :: EventM Name State ()
toggleTable = do
    rst <- use sRowStore
    idx <- use sIndex
    fld <- use sCurrentField
    use (sInterface . levelTable) >>= \case
                Just _ -> do
                            sInterface . levelRows .= Just (mkRowViewer rst idx)
                            modify $ moveFieldTo fld
                Nothing -> do
                             sInterface . levelTable .= Just (buildTable rst idx)
                             modify $ moveTo idx
                             modify $ moveFieldTo fld

moveSearchList :: Event -> EventM Name State ()
moveSearchList e = B.zoom (sInterface . levelSearch . _Just . sdValues) $ handleListEvent e

moveToSelected :: EventM Name State ()
moveToSelected = do
    msd <- use $ sInterface . levelSearch
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
handleInSearchDialog ev = B.zoom (sInterface . levelSearch . _Just . sdDialog) $ handleDialogEvent ev

moveTo :: Int -> State -> State
moveTo pos s
  | valid = over sInterface moveInterface indexUpdated
  | otherwise = s
  where valid = 0 <= pos && pos < size (s ^. sRowStore)
        indexUpdated = set sIndex pos s
        moveInterface = updateLevels mi
        mi se@(Searching _ _) = se
        mi (Zoomed zv i) = Zoomed (updateZoomViewer (currentField indexUpdated) zv) i
        mi (RichZoomed iv i) = RichZoomed (updateRichZoomViewer (currentField indexUpdated) iv) i
        mi (AsTable tv) = AsTable (over tvLists (listMoveTo pos) tv)
        mi (AsRows rv) = let
                           vList = case listSelected (rv ^. rvFieldNames) of
                                     Nothing -> valueList pos (s ^. sRowStore)
                                     Just n -> listMoveTo n (valueList pos (s ^. sRowStore))
                         in AsRows (set rvValueList vList rv)


moveFieldTo :: Int -> State -> State
moveFieldTo pos s
  | valid = over sInterface moveInterface posUpdated
  | otherwise = s
    where valid = 0 <= pos && pos < length (fnames $ s ^. sRowStore)
          posUpdated = set sCurrentField pos s
          moveInterface = updateLevels mi
          mi (Searching sd i) = Searching sd i
          mi (Zoomed _ i) = Zoomed (mkZoomViewer posUpdated) i
          mi (RichZoomed _ i) = RichZoomed (mkRichZoomViewer posUpdated) i
          mi (AsTable tv) = AsTable (set tvCurrentField pos tv)
          mi (AsRows rv) = AsRows (over rvFieldNames (listMoveTo pos) $
                                        over rvValueList (listMoveTo pos) rv)

selectedElementAttr :: AttrName
selectedElementAttr = attrName "selectedElement"

titleAttr :: AttrName
titleAttr = attrName "title"

formulaAttr :: AttrName
formulaAttr = attrName "formula"

errorAttr :: AttrName
errorAttr = attrName "error"

myAttrMap :: AttrMap
myAttrMap = attrMap defAttr [ (selectedElementAttr, withStyle defAttr reverseVideo)
                            , (buttonSelectedAttr, withStyle defAttr reverseVideo)
                            , (titleAttr, withStyle defAttr bold)
                            , (formulaAttr, withBackColor (withForeColor defAttr black) $ rgbColor (160 :: Int) 255 255)
                            , (errorAttr, withBackColor (withForeColor defAttr black) $ rgbColor 255 (160 :: Int) 255)
                            ]

doSave :: EventM Name State ()
doSave = do
    msi <- use sSourceInfo
    case msi of
        Nothing -> logMessage "No source info available"
        Just (si, sis) -> do
            rst <- use sRowStore
            liftIO $ writeRowStore si sis rst
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
