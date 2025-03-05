{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module TUI (
  startTUI
) where

import Brick hiding (getName, zoom)
import Brick qualified as B
import Brick.Widgets.Border
import Brick.Widgets.Center
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
import Graphics.Vty.Attributes(defAttr, bold, reverseVideo, withStyle)
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl))

import Model.Field
import Model.Expression.RecursionSchemas
import Model.RowStore
import Graphics.Vty (imageWidth, imageHeight, translate)
import Control.Monad (when)

maxWidth :: Int
maxWidth = 40

data Name = DButton DialogButton
          | FieldNames
          | SearchList
          | ValueColumn Int
          | ValueEditor Int
          | ValueList
          | ZoomEditor
          deriving (Eq, Ord, Show)

data DialogButton = OkButton | CancelButton deriving (Eq, Ord, Show)

data Level i = Searching SearchDialog i | Zoomed ZoomViewer i | AsTable TableViewer | AsRows RowViewer deriving Functor

type Interface = Fix Level


mkAsRows :: RowViewer -> Interface
mkAsRows = In . AsRows

mkAsTable :: TableViewer -> Interface
mkAsTable = In . AsTable


data State = State { _sRowStore :: RowStore
                   , _sIndex :: Int
                   , _sCurrentField :: Int
                   , _sInterface :: Interface
                   }

type ValueEditor = Ed.Editor Text

data SearchDialog = SearchDialog { _sdValues :: List Name Text
                                 , _sdDialog :: Dialog () Name
                                 }

data RowViewer = RowViewer { _rvFieldNames :: List Name Text
                           , _rvFieldWidth :: Int
                           , _rvValueList :: List Name (ValueEditor Name)
                           }

data TableViewer = TableViewer { _tvFieldNames :: [Text]
                               , _tvColWidths :: [Int]
                               , _tvColumns :: [List Name Text]
                               , _tvCurrentField :: Int
                               }

data ZoomViewer = ZoomViewer { _zvTitle :: Text
                             , _zvEditor :: ValueEditor Name
                             }

rvNames :: Traversal' RowViewer (List Name Text)
rvNames f (RowViewer fl fw vl) = RowViewer <$> f fl <*> pure fw <*> pure vl

rvValues :: Traversal' RowViewer (List Name (ValueEditor Name))
rvValues f (RowViewer fl fw vl) = RowViewer fl fw <$> f vl

tvLists :: Traversal' TableViewer (List Name Text)
tvLists f (TableViewer fl cw cs cf) = TableViewer fl cw <$> traverse f cs <*> pure cf


makeLenses ''RowViewer
makeLenses ''SearchDialog
makeLenses ''State
makeLenses ''TableViewer
makeLenses ''ZoomViewer

updateRvValues :: [Text] -> RowViewer -> RowViewer
updateRvValues ts rv = over rvValueList (listReplace v i) rv
  where i = listSelected $ rv ^. rvValueList
        l = listElements (rv ^. rvValueList)
        v = V.fromList $ zipWith updateEditor ts (V.toList l)

levelSearch :: Lens' Interface (Maybe SearchDialog)
levelSearch = lens getter setter
    where getter = cata gd
          gd (Searching sd _) = Just sd
          gd (Zoomed _ msd) = msd
          gd _ = Nothing

          setter i Nothing = cata remD i
          setter i (Just sd) = cata (addD sd) i
          remD (Searching _ i) = i
          remD (Zoomed z i) = In $ Zoomed z i
          remD b = In b
          addD :: SearchDialog -> Level Interface -> Interface
          addD _ (Searching _ i) = i
          addD sd (Zoomed z (In (Searching _ i))) = In $ Searching sd (In $ Zoomed z i)
          addD _ z@(Zoomed _ _) = In z
          addD sd b = In $ Searching sd (In b)

levelZoom :: Lens' Interface (Maybe ZoomViewer)
levelZoom = lens getter setter
    where getter = cata gz
          gz (Searching _ mz) = mz
          gz (Zoomed z _) = Just z
          gz _ = Nothing

          setter i Nothing = cata remZ i
          setter i (Just z) = cata (addZ z) i
          remZ (Searching sd i) = In $ Searching sd i
          remZ (Zoomed _ i) = i
          remZ b = In b
          addZ :: ZoomViewer -> Level Interface -> Interface
          addZ _ (Searching sd i) = In $ Searching sd i
          addZ _ (Zoomed _ i) = i
          addZ z b = In $ Zoomed z (In b)

levelTable :: Lens' Interface (Maybe TableViewer)
levelTable = lens getter setter
    where getter = cata gt
          gt (Searching _ mtv) = mtv
          gt (Zoomed _ mtv) = mtv
          gt (AsTable tv) = Just tv
          gt _ = Nothing

          setter _ Nothing = error "Cannot remove table: add a row viewer"
          setter i (Just tv) = cata (addT tv) i
          addT :: TableViewer -> Level Interface -> Interface
          addT _ s@(Searching _ _) = In s
          addT _ z@(Zoomed _ _) = In z
          addT tv _ = In $ AsTable tv

levelRows :: Lens' Interface (Maybe RowViewer)
levelRows = lens getter setter
    where getter = cata gr
          gr (Searching _ mrv) = mrv
          gr (Zoomed _ mrv) = mrv
          gr (AsTable _) = Nothing
          gr (AsRows rv) = Just rv

          setter _ Nothing = error "Cannot remove row viewer: add a table"
          setter i (Just rv) = cata (addR rv) i
          addR :: RowViewer -> Level Interface -> Interface
          addR _ s@(Searching _ _) = In s
          addR _ z@(Zoomed _ _) = In z
          addR rv _ = In $ AsRows rv

activeEditor :: Lens' Interface (Maybe (ValueEditor Name))
activeEditor = lens getter setter
    where getter = cata ae
          ae (Searching _ _) = Nothing
          ae (Zoomed z _) = Just $ z ^. zvEditor
          ae (AsTable _) = Nothing
          ae (AsRows rv) = case listSelectedElement (rv ^. rvValueList) of
                             Nothing -> Nothing
                             Just (_, ve) -> Just ve

          setter _ Nothing = error "Cannot remove editor"
          setter i (Just ve) = para (addE ve) i
          addE :: ValueEditor Name -> Level (Interface, Interface) -> Interface
          addE _ (Searching sd (_, i)) = In $ Searching sd i
          addE ve (Zoomed zv (i, _)) = In $ Zoomed (set zvEditor ve zv) i
          addE _ (AsTable _) = error "Cannot add editor to table"
          addE ve (AsRows rv) = In $ AsRows (set (rvValueList . element (fromMaybe 0 $ listSelected $ rv ^. rvValueList)) ve rv)


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
renderZoomViewer zv = myCenter $ joinBorders $
                       hLimitPercent 95 $
                       borderWithLabel (withAttr titleAttr $ txt $ zv ^. zvTitle) $
                         Ed.renderEditor (txt . T.unlines) True (zv ^. zvEditor)
                         <=>
                         hBorder
                         <=>
                         hCenter (txt "C-z: close zoom")

-- handleSearchDialogEvent :: Event -> SearchDialog n -> EventM n (SearchDialog n)
-- handleSearchDialogEvent = traverseOf sdDialog . handleDialogEvent


initialState :: RowStore -> State
initialState rst = State { _sRowStore = rst
                         , _sIndex = 0
                         , _sCurrentField = 0
                         , _sInterface = mkAsRows $ mkRowViewer rst 0
                         }

currentFieldName :: State -> Text
currentFieldName s = fnames (s ^. sRowStore) !! (s ^. sCurrentField)

currentFieldValue :: State -> Text
currentFieldValue s = toString $ row (s ^. sIndex) (s ^. sRowStore) !! (s ^. sCurrentField)

mkRowViewer :: RowStore -> Int -> RowViewer
mkRowViewer rst pos = RowViewer { _rvFieldNames = listMoveTo pos fl
                                , _rvFieldWidth = min maxWidth (V.maximum . V.map T.length $ listElements fl)
                                , _rvValueList = valueList pos rst
                                }
                            where fl = fieldList rst

mkZoomViewer :: Text -> Text -> ZoomViewer
mkZoomViewer title value = ZoomViewer title (mkEditor ZoomEditor value)

mkEditor :: Name -> Text -> ValueEditor Name
mkEditor n = Ed.editor n (Just 1)

fieldList :: RowStore -> List Name Text
fieldList rst = list FieldNames (V.fromList $ fnames rst) 1

valueList :: Int -> RowStore -> List Name (ValueEditor Name)
valueList pos rst = list ValueList (V.fromList $ zipWith valueEditor [0..] (row pos rst)) 1
    where valueEditor n t = mkEditor (ValueEditor n) (toString t)


buildTable :: RowStore -> Int -> TableViewer
buildTable rst = TableViewer ns ws cls
    where cs = transpose $ map (map toString) $ rows rst
          cls = zipWith f [0..] cs
          f n r = list (ValueColumn n) (V.fromList r) 1
          ns = fnames rst
          ws = zipWith max (map T.length ns)
                           $ map (maximum . map T.length) cs


type EventType = ()


draw :: State -> [Widget Name]
draw s = cata doDraw $ _sInterface s
    where
        doDraw (Searching sd ws) = renderSearchDialog sd : ws
        doDraw (Zoomed zv ws) = renderZoomViewer zv : ws
        doDraw (AsTable tv) = [renderBack (title s) (renderTableViewer tv) tableHelp]
        doDraw (AsRows rv) = [renderBack (title s) (renderRowViewer rv) rowHelp]
        tableHelp = "C-z: toggle zoom, C-t: return to field view, C-f: find, C-q: exit"
        rowHelp = "C-z: toggle zoom, C-t: table view, C-f: find, C-q: exit"


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


title :: State -> Text
title s = T.concat [ getName $ s ^. sRowStore
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
                , renderList renderValueEditor False $ rv ^. rvValueList
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

renderValueEditor :: Bool -> ValueEditor Name -> Widget Name
renderValueEditor False = Ed.renderEditor (txt . T.unlines) True
renderValueEditor True = Ed.renderEditor (txt . T.unlines) True

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


myCenter :: Widget Name -> Widget Name
myCenter w = Widget Fixed Fixed $ do
    wd <- availWidth <$> getContext
    hg <- availHeight <$> getContext
    r <- render w
    let im = image r
        rw = imageWidth im
        rh = imageHeight im
        hoffset = (wd - rw) `div` 2
        voffset = (hg - rh) `div` 2
    return r { image = translate hoffset voffset im }

app :: App State EventType Name
app = App { appDraw = draw
          , appChooseCursor = showSelectedCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const myAttrMap
          }

showSelectedCursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
showSelectedCursor s cs = cata findCursor $ s ^. sInterface
    where findCursor (Searching _ mc) = mc
          findCursor (Zoomed _ _) = find ((== Just ZoomEditor) . cursorLocationName) cs
          findCursor (AsTable _) = Nothing
          findCursor (AsRows rv) = case listSelected (rv ^. rvValueList) of
                                      Nothing -> Nothing
                                      Just n -> find ((== Just (ValueEditor n)) . cursorLocationName) cs

listKeys :: [Key]
listKeys = [KDown, KUp, KPageUp, KPageDown, KHome, KEnd, KLeft, KRight]

(>>->>) :: EventM Name State Bool -> EventM Name State () -> EventM Name State ()
(>>->>) e1 e2 = e1 >>= \case
                    True -> return ()
                    False -> e2

handleEvent :: BrickEvent Name EventType -> EventM Name State ()
handleEvent e = handleGlobalEvent e
   >>->> (use sInterface >>= (\case
                Searching _ _ -> handleEventSearch e
                Zoomed _ _ -> handleEventZoom e
                AsTable _ -> handleEventTable e
                AsRows _ -> handleEventRows e) . out)

handleGlobalEvent :: BrickEvent Name EventType -> EventM Name State Bool
handleGlobalEvent (VtyEvent (EvKey (KChar 'q') [MCtrl])) = halt >> return True
handleGlobalEvent _ = return False

handleEventSearch :: BrickEvent Name EventType -> EventM Name State ()
handleEventSearch (VtyEvent (EvKey k ms)) = handleKeySearch k ms
handleEventSearch _ = return ()

handleKeySearch :: Key -> [Modifier] -> EventM Name State ()
handleKeySearch k [] | k `elem` listKeys = moveSearchList (EvKey k [])
handleKeySearch KEnter [] = moveToSelected
handleKeySearch KEsc [] = deactivateSearch
handleKeySearch k ms = handleInSearchDialog (EvKey k ms)


handleCommonKeys :: BrickEvent Name EventType -> EventM Name State Bool
handleCommonKeys (VtyEvent (EvKey (KChar c) [MCtrl])) = case c of
    'f' -> activateSearch >> return True
    't' -> toggleTable >> return True
    'z' -> toggleZoom >> return True
    _ -> return False
handleCommonKeys (VtyEvent (EvKey k [MCtrl])) = case k of
    KUp -> backward >> return True
    KDown -> forward >> return True
    KLeft -> fieldBackward >> return True
    KRight -> fieldForward >> return True
    _ -> return False
handleCommonKeys _ = return False

handleEventZoom :: BrickEvent Name EventType -> EventM Name State ()
handleEventZoom e = handleCommonKeys e >>->> case e of
    VtyEvent (EvKey KPageUp []) -> backward
    VtyEvent (EvKey KPageDown []) -> forward
    VtyEvent (EvKey KUp []) -> fieldBackward
    VtyEvent (EvKey KDown []) -> fieldForward
    VtyEvent (EvKey KEnter []) -> fieldForward
    _ -> handleEdition e

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
    _ -> handleEdition e

handleEdition :: BrickEvent Name EventType -> EventM Name State ()
handleEdition e = do
    med <- use (sInterface . activeEditor)
    case med of
        Nothing -> return ()
        Just ed -> do
                     B.zoom (sInterface . activeEditor . _Just) $ Ed.handleEditorEvent e
                     value <- use (sInterface . activeEditor . _Just . to Ed.getEditContents)
                     updateCurrentField $ T.concat value

updateCurrentField :: Text -> EventM Name State ()
updateCurrentField t = do
    s <- get
    let (rst, ch) = changeField (s ^. sIndex) (fromIntegral $ s ^. sCurrentField) (toField t) (s ^. sRowStore)
    when (notNull ch) $ do
        sRowStore .= rst
        let r = map toString $ row (s ^. sIndex) rst
        sInterface %= cata (uRow r (r !! (s ^. sCurrentField)))
  where
    uRow _ _ s@(Searching _ _) = In s
    uRow _ ft (Zoomed zv i) = In (Zoomed (over zvEditor (updateEditor ft) zv) i)
    uRow r ft (AsTable tv) = In $ AsTable tv
    uRow r _ (AsRows rv) = In . AsRows $ updateRvValues r rv

notNull :: [a] -> Bool
notNull [] = False
notNull _ = True

updateEditor :: Text -> ValueEditor Name -> ValueEditor Name
updateEditor t ed | t == T.concat (Ed.getEditContents ed) = ed
                  | otherwise = Ed.applyEdit (const $ Tz.textZipper [t] $ Just 1) ed

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
zoom s = set (sInterface . levelZoom) (Just z) s
    where z = mkZoomViewer (currentFieldName s) (currentFieldValue s)


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
{-

sInterface %= cata tTable
    where tTable z@(Zoomed _ _) = z
          tTable s@(Searching _ _) = s
          tTable (AsTable _) = In . mkAsRows $ mkRowViewer (sRowStore s) (sIndex s)
          tTable (AsRows _) = In . mkAsTable $ buildTable (sRowStore s)
          -}

moveLists :: Event -> EventM Name State ()
moveLists e = return ()
{-
moveLists e = use sInterface >>= (\case
                   AsTable -> moveListsTables e
                   AsRows -> moveListsRows e
                   _ -> error "Impossible"
                   ) . baseAppearance

-}
{-
moveListsRows :: Event -> EventM Name State ()
moveListsRows (EvKey KPageUp []) = backward
moveListsRows (EvKey KPageDown []) = forward
moveListsRows e = do
    B.zoom (sRowViewer . rvNames) $ handleListEvent e
    B.zoom (sRowViewer . rvValues) $ handleListEvent e
    cf <- uses (sRowViewer . rvFieldNames) (fromMaybe 0 . listSelected)
    sCurrentField .= cf
    B.zoom (sRowViewer . rvValues . listSelectedElementL) $ Ed.handleEditorEvent (VtyEvent e)
    modify updateZoom


moveListsTables :: Event -> EventM Name State ()
moveListsTables (EvKey KLeft []) = moveListsRows (EvKey KUp [])
moveListsTables (EvKey KRight []) = moveListsRows (EvKey KDown [])
moveListsTables e = do
    cols <- use (sTableViewer . tvColumns)
    let col = head cols
    (col', ()) <- nestEventM col $ handleListEvent e
    sTableViewer . tvColumns .= col' : tail cols
    modify $ updateZoom . moveTo (fromMaybe 0 $ listSelected col')
    -}

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
        moveInterface = cata mi
        mi (Searching sd i) = In $ Searching sd i
        mi (Zoomed zv i) = In $ Zoomed (set zvEditor (mkEditor ZoomEditor (currentFieldValue indexUpdated)) zv) i
        mi (AsTable tv) = In $ AsTable (over tvLists (listMoveTo pos) tv)
        mi (AsRows rv) = let
                           vList = case listSelected (rv ^. rvFieldNames) of
                                     Nothing -> valueList pos (s ^. sRowStore)
                                     Just n -> listMoveTo n (valueList pos (s ^. sRowStore))
                         in In $ AsRows (set rvValueList vList rv)

moveFieldTo :: Int -> State -> State
moveFieldTo pos s
  | valid = over sInterface moveInterface posUpdated
  | otherwise = s
    where valid = 0 <= pos && pos < length (fnames $ s ^. sRowStore)
          posUpdated = set sCurrentField pos s
          moveInterface = cata mi
          mi (Searching sd i) = In $ Searching sd i
          mi (Zoomed zv i) = In $ Zoomed (mkZoomViewer (currentFieldName posUpdated) (currentFieldValue posUpdated)) i
          mi (AsTable tv) = In $ AsTable (set tvCurrentField pos tv)
          mi (AsRows rv) = In $ AsRows (over rvFieldNames (listMoveTo pos) $
                                        over rvValueList (listMoveTo pos) rv)

selectedElementAttr :: AttrName
selectedElementAttr = attrName "selectedElement"

titleAttr :: AttrName
titleAttr = attrName "title"

myAttrMap :: AttrMap
myAttrMap = attrMap defAttr [ (selectedElementAttr, withStyle defAttr reverseVideo)
                            , (buttonSelectedAttr, withStyle defAttr reverseVideo)
                            , (titleAttr, withStyle defAttr bold)
                            ]



startTUI :: RowStore -> IO ()
startTUI rst = do
  _ <- defaultMain app (initialState rst)
  return ()

