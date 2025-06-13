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
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core qualified as BC
import Brick.Widgets.Dialog
import Brick.Widgets.Edit qualified as Ed
import Brick.Widgets.List hiding (splitAt, reverse)
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (try, SomeException)
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.List(transpose, intersperse, find)
import Data.Maybe(fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as Tz
import Data.Vector qualified as V
import Graphics.Vty (imageWidth, imageHeight, translate, Vty (outputIface), supportsMode)
import Graphics.Vty.Attributes (defAttr, bold, reverseVideo, withStyle, withBackColor, withForeColor, black, rgbColor)
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

maxWidth :: Int
maxWidth = 40

data BackupEvent = BackupEvent deriving (Show)

data Name = DButton DialogButton
          | FieldNames
          | SearchList
          | RichZoomValueEditor
          | RichZoomFormulaEditor
          | ValueColumn Int
          | ValueViewer Int
          | ValueList
          | ZoomEditor
          deriving (Eq, Ord, Show)

data DialogButton = OkButton | CancelButton deriving (Eq, Ord, Show)

data Level i = Searching SearchDialog i
             | Zoomed ZoomLevel i
             | Back BackLevel deriving Functor

data ZoomLevel = NormalZoom ZoomViewer
               | RichZoom RichZoomViewer

data BackLevel = AsTable TableViewer
               | AsRows RowViewer

isSearching :: Level i -> Bool
isSearching (Searching _ _) = True
isSearching _ = False

isZoomed :: Level i -> Bool
isZoomed (Zoomed _ _) = True
isZoomed _ = False

isBack :: Level i -> Bool
isBack (Back _) = True
isBack _ = False

type Interface = Fix Level

class HasEditor i where
    editorLens :: Lens' i (Maybe ValueEditor)

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

data RichZoomFocus = RzValue | RzType | RzFMark | RzFormula deriving (Eq, Ord, Enum)

data RichZoomViewer = RichZoomViewer { _ivTitle :: Text
                                     , _ivValue :: ValueViewer
                                     , _ivType :: Text
                                     , _ivIsFormula :: Bool
                                     , _ivFormula :: ValueEditor
                                     , _ivFocus :: RichZoomFocus
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

instance HasEditor ZoomViewer where
    editorLens = lens getter setter
        where getter zv = case zv ^. zvValue of
                             Left ve -> Just ve
                             _ -> Nothing
              setter _ Nothing = error "Cannot remove editor from zoom viewer"
              setter zv (Just ve) = set zvValue (Left ve) zv

instance HasEditor RichZoomViewer where
    editorLens = lens getter setter
        where getter iv = case iv ^. ivFocus of
                              RzValue -> case iv ^. ivValue of
                                             Left ve -> Just ve
                                             _ -> Nothing
                              RzFormula -> if iv ^. ivIsFormula
                                             then Just $ iv ^. ivFormula
                                             else Nothing
                              _ -> Nothing
              setter _ Nothing = error "Cannot remove editor from rich zoom viewer"
              setter iv (Just ve) = case iv ^. ivFocus of
                                      RzValue -> set ivValue (Left ve) iv
                                      RzFormula -> set ivFormula ve iv
                                      _ -> error "Bad focus in rich zoom for setting editor"

instance HasEditor ZoomLevel where
    editorLens = lens getter setter
        where getter (NormalZoom zv) = zv ^. editorLens
              getter (RichZoom iv) = iv ^. editorLens
              setter (NormalZoom zv) v = NormalZoom $ set editorLens v zv
              setter (RichZoom zv) v = RichZoom $ set editorLens v zv


instance HasEditor RowViewer where
    editorLens = lens getter setter
        where getter rv = do
                             (_, Left ve) <- listSelectedElement $ rv ^. rvValueList
                             return ve
              setter _ Nothing = error "Cannot remove editor from row viewer"
              setter rv (Just ve) = set (rvValueList . element (fromMaybe 0 $ listSelected $ rv ^. rvValueList)) (Left ve) rv

instance HasEditor TableViewer where
    editorLens = lens (const Nothing) setter
        where setter _ Nothing = error "Cannot remove editor from table viewer"
              setter _ _ = error "Cannot set editor in table viewer"

instance HasEditor BackLevel where
    editorLens = lens getter setter
        where getter (AsTable tv) = tv ^. editorLens
              getter (AsRows rv) = rv ^. editorLens
              setter (AsTable tv) v = AsTable $ set editorLens v tv
              setter (AsRows rv) v = AsRows $ set editorLens v rv

updateRvValues :: [Field] -> RowViewer -> RowViewer
updateRvValues ts rv = over rvValueList (listReplace v i) rv
  where i = listSelected $ rv ^. rvValueList
        l = listElements (rv ^. rvValueList)
        v = V.fromList $ zipWith updateValueViewer ts (V.toList l)

getLevel :: (forall i . Level i -> Bool) -> Interface -> Maybe Interface
getLevel f = para search
    where search :: Level (Interface, Maybe Interface) -> Maybe Interface
          search l@(Searching sd (i, ms)) = if f l then Just (In $ Searching sd i) else ms
          search l@(Zoomed zl (i, _)) = if f l then Just (In $ Zoomed zl i) else Nothing
          search l@(Back bl) = if f l then Just (In $ Back bl) else Nothing

removeLevel :: (forall i . Level i -> Bool) -> Interface -> Interface
removeLevel f = updateLevels remL
    where remL l@(Searching _ i) = if f l then out i else l
          remL l@(Zoomed _ i) = if f l then out i else l
          remL l@(Back _) = if f l then error "Cannot remove back" else l

levelSearch :: Lens' Interface (Maybe SearchDialog)
levelSearch = lens getter setter
    where getter i = case getLevel isSearching i of
                         Just (In (Searching sd _)) -> Just sd
                         _ -> Nothing

          setter i Nothing = removeLevel isSearching i
          setter i (Just sd) = updateLevels (addD sd) i
          addD :: SearchDialog -> Level Interface -> Level Interface
          addD _ (Searching _ i) = out i
          addD sd (Zoomed zl (In (Searching _ i))) = Searching sd (In $ Zoomed zl i)
          addD _ z@(Zoomed _ _) = z
          addD sd t@(Back _) = Searching sd (In t)

levelZoom :: Lens' Interface (Maybe ZoomLevel)
levelZoom = lens getter setter
    where getter i = case getLevel isZoomed i of
                         Just (In (Zoomed zl _)) -> Just zl
                         _ -> Nothing

          setter i Nothing = removeLevel isZoomed i
          setter i (Just z) = updateLevels (addZ z) i
          addZ :: ZoomLevel -> Level Interface -> Level Interface
          addZ _ s@(Searching _ _) = s
          addZ _ (Zoomed _ i) = out i
          addZ z t@(Back _) = Zoomed z (In t)

levelBack :: Lens' Interface (Maybe BackLevel)
levelBack = lens getter setter
    where getter i = do
                       In (Back bl) <- getLevel isBack i
                       return bl
          setter i Nothing = removeLevel isBack i
          setter i (Just bl) = updateLevels (addB bl) i
          addB :: BackLevel -> Level Interface -> Level Interface
          addB _ s@(Searching _ _) = s
          addB _ z@(Zoomed _ _) = z
          addB bl (Back _) = Back bl

richZoom :: Lens' Interface (Maybe RichZoomViewer)
richZoom = lens getter setter
    where getter i = do
                        RichZoom iv <- i ^. levelZoom
                        return iv

          setter i v = set levelZoom (fmap RichZoom v) i

tableViewer :: Lens' Interface (Maybe TableViewer)
tableViewer = lens getter setter
    where getter i = do
                        AsTable tv <- i ^. levelBack
                        return tv
          setter i v = set levelBack (fmap AsTable v) i

rowViewer :: Lens' Interface (Maybe RowViewer)
rowViewer = lens getter setter
    where getter i = do
                        AsRows rv <- i ^. levelBack
                        return rv
          setter i v = set levelBack (fmap AsRows v) i

activeEditor :: Lens' Interface (Maybe ValueEditor)
activeEditor = lens getter setter
    where getter = cata ae
          ae (Searching _ _) = Nothing
          ae (Zoomed zl _) = zl ^. editorLens
          ae (Back bl) = bl ^. editorLens

          setter _ Nothing = error "Cannot remove editor"
          setter i (Just ve) = para (addE ve) i
          addE :: ValueEditor -> Level (Interface, Interface) -> Interface
          addE _ (Searching sd (_, i)) = In $ Searching sd i
          addE ve (Zoomed zl (i, _)) = In $ Zoomed (set editorLens (Just ve) zl) i
          addE ve (Back bl) = In $ Back $ set editorLens (Just ve) bl


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
                          wFocus RzType (txt $ iv ^. ivType)
                         <=>
                         hBorder
                         <=>
                         if iv ^. ivIsFormula
                         then wFocus RzFMark (txt "[X]") <+> txt " " <+> renderValueEditor (iv ^. ivFormula)
                         else wFocus RzFMark (txt "[ ]")
                         <=>
                         hBorder
                         <=>
                         hCenter (txt "C-r: close rich zoom, C-z: zoom")
            where wFocus n = if iv ^. ivFocus == n
                      then withAttr selectedElementAttr
                      else id


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
                             else Left (mkEditor RichZoomValueEditor $ currentField s)
                             )
                            (currentFieldType s)
                            (isJust $ currentFieldFormula s)
                            (mkEditor RichZoomFormulaEditor $ toField $ fromMaybe "" (currentFieldFormula s))
                            RzValue

updateZoomViewer :: Field -> ZoomViewer -> ZoomViewer
updateZoomViewer f = over zvValue (either (Left . updateEditor f) (Right . const f))

updateRichZoomViewer :: Field -> RichZoomViewer -> RichZoomViewer
updateRichZoomViewer f = over ivValue (either (Left . updateEditor f) (Right . const f))

richZoomMoveUp :: RichZoomViewer -> RichZoomViewer
richZoomMoveUp = over ivFocus (\i -> if i > RzValue then pred i else RzValue)

richZoomMoveDown :: RichZoomViewer -> RichZoomViewer
richZoomMoveDown rz = over ivFocus (\i -> if i < limit then succ i else limit) rz
  where
    limit = if rz ^. ivIsFormula then RzFormula else RzFMark

richZoomNextType :: RichZoomViewer -> Maybe (RichZoomViewer, FieldType)
richZoomNextType rz
   | rz ^. ivFocus /= RzType || t == maxBound || t' == TypeEmpty = Nothing
   | otherwise = Just (set ivType (T.pack $ show t') rz, t')
   where t = read . T.unpack $ rz ^. ivType
         t' = succ t

richZoomPrevType :: RichZoomViewer -> Maybe (RichZoomViewer, FieldType)
richZoomPrevType rz
   | rz ^. ivFocus /= RzType || t == minBound = Nothing
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
        doDraw (Zoomed (NormalZoom zv) ws) = renderZoomViewer zv : ws
        doDraw (Zoomed (RichZoom iv) ws) = renderRichZoomViewer iv : ws
        doDraw (Back (AsTable tv)) = [renderBack (windowTitle s) (renderTableViewer tv) tableHelp]
        doDraw (Back (AsRows rv)) = [renderBack (windowTitle s) (renderRowViewer rv) rowHelp]
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
handleInLevel e (Searching _ _) = handleEventSearch e
handleInLevel e (Zoomed (NormalZoom _) i) = handleInLevel e $ out i
handleInLevel e (Zoomed (RichZoom _) i) = handleEventRichZoom e >>->> handleInLevel e (out i)
handleInLevel e (Back (AsTable _)) = handleEventTable e
handleInLevel e (Back (AsRows _)) = handleEventRows e

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
    Just rz -> if rz ^. ivFocus == RzFMark
               then do
                      sInterface . richZoom . _Just . ivIsFormula %= not
                      f <- if rz ^. ivIsFormula
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
                        Just rz -> if rz ^. ivFocus == RzValue
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
    uRow _ _ s@(Searching _ _) = s
    uRow _ ft (Zoomed (NormalZoom zv) i) = Zoomed (NormalZoom (over zvValue (updateValueViewer ft) zv)) i
    uRow _ ft (Zoomed (RichZoom iv) i) = Zoomed (RichZoom (over ivValue (updateValueViewer ft) iv)) i
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
zoom s = set (sInterface . levelZoom) (Just . NormalZoom $ mkZoomViewer s) s

toggleInfo :: EventM Name State ()
toggleInfo = use (sInterface . richZoom) >>= \case
                Just _ -> sInterface . richZoom .= Nothing
                Nothing -> modify info

info :: State -> State
info s = set (sInterface . richZoom) (Just $ mkRichZoomViewer s) s

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
    use (sInterface . tableViewer) >>= \case
                Just _ -> do
                            sInterface . rowViewer .= Just (mkRowViewer rst idx)
                            modify $ moveFieldTo fld
                Nothing -> do
                             sInterface . tableViewer .= Just (buildTable rst idx)
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
        mi (Zoomed (NormalZoom zv) i) = Zoomed (NormalZoom (updateZoomViewer (currentField indexUpdated) zv)) i
        mi (Zoomed (RichZoom iv) i) = Zoomed (RichZoom (updateRichZoomViewer (currentField indexUpdated) iv)) i
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
          mi (Searching sd i) = Searching sd i
          mi (Zoomed (NormalZoom _) i) = Zoomed (NormalZoom $ mkZoomViewer posUpdated) i
          mi (Zoomed (RichZoom _) i) = Zoomed (RichZoom $ mkRichZoomViewer posUpdated) i
          mi (Back (AsTable tv)) = Back $ AsTable (set tvCurrentField pos tv)
          mi (Back (AsRows rv)) = Back $ AsRows (over rvFieldNames (listMoveTo pos) $
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
