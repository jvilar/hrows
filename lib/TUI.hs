{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module TUI (
  startTUI
) where

import Brick hiding (getName)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Brick.Widgets.List hiding (splitAt, reverse)
import Control.Lens hiding (index, Zoom, zoom)
import Data.List(transpose, intersperse)
import Data.Maybe(isJust, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Graphics.Vty.Attributes(defAttr, bold, reverseVideo, withStyle)
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl))

import Model.RowStore
import Graphics.Vty (imageWidth, imageHeight, translate)

maxWidth :: Int
maxWidth = 40

data Name = FieldNames | ValueList | SearchList | ValueColumn Int deriving (Eq, Ord, Show)

data DialogButton = OkButton | CancelButton

data State = State { _sRowStore :: RowStore
                   , _sIndex :: Int
                   , _sCurrentField :: Int
                   , _sRowViewer :: RowViewer
                   , _sZoom :: Maybe Zoom
                   , _sSearch :: Maybe (SearchDialog Name)
                   , _sTableViewer :: TableViewer
                   , _sIsTable :: Bool
                   }

type Zoom = (Text, Text)

data SearchDialog n = SearchDialog { _sdValues :: List n Text
                                   , _sdDialog :: Dialog DialogButton
                                   }

data RowViewer = RowViewer { _rvFieldNames :: List Name Text
                           , _rvFieldWidth :: Int
                           , _rvValueList :: List Name Text
                           }

data TableViewer = TableViewer { _tvFieldNames :: [Text]
                               , _tvColWidths :: [Int]
                               , _tvColumns :: [List Name Text]
                               }

rvLists :: Traversal' RowViewer (List Name Text)
rvLists f (RowViewer fl fw vl) = RowViewer <$> f fl <*> pure fw <*> f vl

tvLists :: Traversal' TableViewer (List Name Text)
tvLists f (TableViewer fl cw cs) = TableViewer fl cw <$> traverse f cs

makeLenses ''RowViewer
makeLenses ''SearchDialog
makeLenses ''State
makeLenses ''TableViewer

searchDialog :: n -> Int -> Text -> [Text] -> SearchDialog n
searchDialog n w ttle values = SearchDialog (list n (V.fromList values) 1)
                                            (dialog (Just $ T.unpack ttle)
                                                    (Just (0, [ ("OK", OkButton)
                                                              , ("Cancel", CancelButton)]))
                                                    w
                                            )


renderSearchDialog :: SearchDialog Name -> Widget Name
renderSearchDialog sd = renderDialog (sd ^. sdDialog) $
                           vLimit (V.length $ listElements $ sd ^. sdValues)
                                  (renderList renderValue False $ sd ^. sdValues)


handleSearchDialogEvent :: Event -> SearchDialog n -> EventM n (SearchDialog n)
handleSearchDialogEvent = traverseOf sdDialog . handleDialogEvent


initialState :: RowStore -> State
initialState rst = State { _sRowStore = rst
                         , _sIndex = 0
                         , _sCurrentField = 0
                         , _sRowViewer = initialRowViewer rst
                         , _sZoom = Nothing
                         , _sSearch = Nothing
                         , _sTableViewer = buildTable rst
                         , _sIsTable = False
                         }

initialRowViewer :: RowStore -> RowViewer
initialRowViewer rst = RowViewer { _rvFieldNames = listMoveTo 0 fl
                                 , _rvFieldWidth = min maxWidth (V.maximum . V.map T.length $ listElements fl)
                                 , _rvValueList = valueList 0 rst
                                 }
                            where fl = fieldList rst

fieldList :: RowStore -> List Name Text
fieldList rst = list FieldNames (V.fromList $ fnames rst) 1

valueList :: Int -> RowStore -> List Name Text
valueList pos rst = list ValueList (V.fromList $ map toString $ row pos rst) 1


buildTable :: RowStore -> TableViewer
buildTable rst = TableViewer ns ws cls
    where cs = transpose $ map (map toString) $ rows rst
          cls = zipWith f [0..] cs
          f n r = list (ValueColumn n) (V.fromList r) 1
          ns = fnames rst
          ws = zipWith max (map T.length ns)
                           $ map (maximum . map T.length) cs


type EventType = ()


draw :: State -> [Widget Name]
draw s = [ renderFront s, renderBack s ]

renderBack :: State -> Widget Name
renderBack s = joinBorders $ center $
       borderWithLabel (withAttr "title" . txt $ title s) $
       content
       <=>
       hBorder
       <=>
       hCenter (txt help)
  where
    content | s ^. sIsTable = renderTableViewer (s^. sCurrentField) (s ^. sTableViewer)
            | otherwise = renderRowViewer (s ^. sRowViewer)
    help    | s ^. sIsTable = "z: toggle zoom, t: return to field view, C-f: find, C-q: exit"
            | otherwise = "z: toggle zoom, t: table view, C-f: find, C-q: exit"


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
                , renderList renderValue False $ rv ^. rvValueList
              ]

renderTableViewer :: Int -> TableViewer -> Widget Name
renderTableViewer curF tv = Widget Greedy Fixed $ do
    h <- availHeight <$> getContext
    w <- subtract (length (tv ^. tvColumns)-1) . availWidth <$> getContext
    let v = min (V.length $ listElements $ head $ tv ^. tvColumns) (h-4)
        ws = allocateWidths curF w $ tv ^. tvColWidths
    render ( vLimit 1 (hBox $ withWidths (\(i, t) -> 
                                              if i == curF
                                              then withAttr "selectedElement" $ myTxt t
                                              else withAttr "title" $ myTxt t)
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
allocateWidths cur w ws
  | w < s = let
               (lft, wCur:rgt) = splitAt cur ws
               w0 = min w wCur
               (ws0, wd0) = ((reverse . upto (w - w0) $ reverse lft) ++ [w0]
                             , sum ws0)
               ws1 = ws0 ++ upto (w - wd0) rgt
            in ws1
  | otherwise = over (taking n traversed) (+ (dw+1))
              $ over (dropping n traversed) (+ dw) ws
  where s = sum ws
        l = length ws
        (dw, n) = (w - s) `divMod` l
        upto _ [] = []
        upto ml (x:xs) = v:upto (ml-v) xs
            where v = min ml x

withWidths :: (a -> Widget Name) -> [Int] -> [a] -> [Widget Name]
withWidths f = (intersperse vBorder .) . zipWith (withWidth f)

withWidth :: (a -> Widget Name) -> Int -> a -> Widget Name
withWidth f w = hLimit w . padRight Max . f

renderName :: Bool -> Text -> Widget Name
renderName = (withAttr "title" .) . renderElement


renderValue :: Bool -> Text -> Widget Name
renderValue = renderElement


renderElement :: Bool -> Text -> Widget Name
renderElement False = myTxt
renderElement True = withAttr "selectedElement" . myTxt


myTxt :: Text -> Widget n
myTxt t = Widget Fixed Fixed $ do
      w <- availWidth <$> getContext
      let l = T.length t
          t' | l <= w = T.justifyLeft w ' ' t
             | otherwise = T.take (w-3) t <> "..."
      render $ txt t'


renderFront :: State -> Widget Name
renderFront s
  | isJust (s ^. sSearch) = renderSearch s
  | isJust (s ^. sZoom)   = renderZoom s
  | otherwise             = emptyWidget


renderZoom :: State -> Widget Name
renderZoom s = case s ^. sZoom of
    Nothing -> emptyWidget
    Just (lbl, t) -> myCenter $ joinBorders $
                       hLimitPercent 95 $
                       borderWithLabel (withAttr "title" $ txt lbl) $
                         txtWrap (if T.null t
                                  then " "
                                  else t)
                         <=>
                         hBorder
                         <=>
                         hCenter (txt "z: close zoom")


renderSearch :: State -> Widget Name
renderSearch = maybe emptyWidget renderSearchDialog . view sSearch


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
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const myAttrMap
          }


listKeys :: [Key]
listKeys = [KDown, KUp, KPageUp, KPageDown, KHome, KEnd, KLeft, KRight]


handleEvent :: State -> BrickEvent Name EventType -> EventM Name (Next State)
handleEvent s (VtyEvent (EvKey k ms)) = handleKey s k ms
handleEvent s _ = continue s

handleKey :: State -> Key -> [Modifier] -> EventM Name (Next State)
handleKey s k m
            | isJust (s ^. sSearch) = handleKeySearch k m s
            | otherwise = handleKeyStandard k m s

handleKeyStandard :: Key -> [Modifier] -> State -> EventM Name (Next State)
handleKeyStandard (KChar 'q') [MCtrl] = halt
handleKeyStandard (KChar 'f') [MCtrl] = activateSearch
handleKeyStandard (KChar 't') [] = toggleTable
handleKeyStandard (KChar 'z') [] = toggleZoom
handleKeyStandard k [] | k `elem` listKeys = moveLists (EvKey k [])
handleKeyStandard _ _ = continue


handleKeySearch :: Key -> [Modifier] -> State -> EventM Name (Next State)
handleKeySearch k [] | k `elem` listKeys = moveSearchList (EvKey k [])
handleKeySearch KEnter [] = moveToSelected
handleKeySearch KEsc [] = deactivateSearch
handleKeySearch k ms = handleInSearchDialog (EvKey k ms)


backward :: State -> EventM Name (Next State)
backward s = continue $ moveTo (s ^. sIndex - 1) s


forward :: State -> EventM Name (Next State)
forward s = continue $ moveTo (s ^. sIndex + 1) s

toggleZoom :: State -> EventM Name (Next State)
toggleZoom s | isNothing (s ^. sZoom) = continue $ zoom s
             | otherwise = continue $ unZoom s

updateZoom :: State -> State
updateZoom s | isNothing (s ^. sZoom) = s
             | otherwise = zoom s

unZoom :: State -> State
unZoom = set sZoom Nothing

zoom :: State -> State
zoom s = set sZoom z s
  where z = do
             lbl <- snd <$> listSelectedElement (s ^. sRowViewer . rvFieldNames)
             t <- snd <$> listSelectedElement (s ^. sRowViewer . rvValueList)
             return (lbl, t)


activateSearch :: State -> EventM Name (Next State)
activateSearch s = do
  let (index, tle) = fromMaybe (0, "") $ do
                        listSelectedElement (s ^. sRowViewer . rvFieldNames)
      vs = fieldValues (fromIntegral index) (s ^. sRowStore)
      sd = searchDialog SearchList maxWidth tle vs
  continue $ set sSearch (Just sd) s


deactivateSearch :: State -> EventM Name (Next State)
deactivateSearch = continue .set sSearch Nothing


toggleTable :: State -> EventM Name (Next State)
toggleTable = continue . over sIsTable not

moveLists :: Event -> State -> EventM Name (Next State)
moveLists e s | s ^. sIsTable = moveListsTables e s
              | otherwise = moveListsRows e s

moveListsRows :: Event -> State -> EventM Name (Next State)
moveListsRows (EvKey KPageUp []) s = backward s
moveListsRows (EvKey KPageDown []) s = forward s
moveListsRows e s = do
    s' <- traverseOf (sRowViewer . rvLists) (handleListEvent e) s
    let cf = fromMaybe 0 $ listSelected (s' ^. sRowViewer . rvFieldNames)
    continue $ updateZoom $ set sCurrentField cf s'

moveListsTables :: Event -> State -> EventM Name (Next State)
moveListsTables (EvKey KLeft []) s = moveListsRows (EvKey KUp []) s
moveListsTables (EvKey KRight []) s = moveListsRows (EvKey KDown []) s
moveListsTables e s = do
    l <- handleListEvent e (head $ s ^. sTableViewer . tvColumns)
    continue $ updateZoom $ moveTo (fromMaybe 0 $ listSelected l) s


moveSearchList :: Event -> State -> EventM Name (Next State)
moveSearchList e s = do
    sd <- traverseOf (_Just . sdValues) (handleListEvent e) $ s ^. sSearch
    continue $ set sSearch sd s


moveToSelected :: State -> EventM Name (Next State)
moveToSelected s = do
    let Just sd = s ^. sSearch
    case dialogSelection $ sd ^. sdDialog of
        Just OkButton -> case listSelectedElement $ sd ^. sdValues of
                             Nothing -> deactivateSearch s
                             Just (_, t) -> let
                                  pos = nextPos (fromIntegral $ s ^. sCurrentField) t (s ^. sIndex) (s ^. sRowStore)
                                in deactivateSearch $ moveTo pos s
        Just CancelButton -> deactivateSearch s
        Nothing -> continue s


handleInSearchDialog :: Event -> State -> EventM Name (Next State)
handleInSearchDialog ev s = do
    sd <- traverse (handleSearchDialogEvent ev) $ s ^. sSearch
    continue $ set sSearch sd s


moveTo :: Int -> State -> State
moveTo pos s
  | valid = updateZoom
          $ set sIndex pos
          $ over (sTableViewer . tvLists) (listMoveTo pos)
          $ set (sRowViewer . rvValueList) vlist s
  | otherwise = s
    where valid = 0 <= pos && pos < size (s ^. sRowStore)
          vl = valueList pos (s ^. sRowStore)
          vlist = case listSelected (s ^. sRowViewer . rvFieldNames) of
                    Nothing -> vl
                    Just n -> listMoveTo n vl


myAttrMap :: AttrMap
myAttrMap = attrMap defAttr [ ("selectedElement", withStyle defAttr reverseVideo)
                            , (buttonSelectedAttr, withStyle defAttr reverseVideo)
                            , ("title", withStyle defAttr bold)
                            ]



startTUI :: RowStore -> IO ()
startTUI rst = do
  _ <- defaultMain app (initialState rst)
  return ()

