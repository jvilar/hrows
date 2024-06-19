{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (
  startTUI
) where

import Brick hiding (getName, zoom)
import Brick qualified as B
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Brick.Widgets.List hiding (splitAt, reverse)
import Control.Lens hiding (index, Zoom, zoom)
import Data.List(transpose, intersperse)
import Data.Maybe(isJust, fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Graphics.Vty.Attributes(defAttr, bold, reverseVideo, withStyle)
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl))

import Model.RowStore
import Graphics.Vty (imageWidth, imageHeight, translate)

maxWidth :: Int
maxWidth = 40

data Name = FieldNames | ValueList | SearchList | ValueColumn Int | DButton DialogButton deriving (Eq, Ord, Show)

data DialogButton = OkButton | CancelButton deriving (Eq, Ord, Show)

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
                                   , _sdDialog :: Dialog () n
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

searchDialog :: Name -> Int -> Text -> [Text] -> SearchDialog Name
searchDialog n w ttle values = SearchDialog (list n (V.fromList values) 1)
                                            (dialog (Just $ txt ttle)
                                                    (Just (DButton OkButton, [ ("OK", DButton OkButton, ())
                                                              , ("Cancel", DButton CancelButton, ())]))
                                                    w
                                            )


renderSearchDialog :: SearchDialog Name -> Widget Name
renderSearchDialog sd = renderDialog (sd ^. sdDialog) $
                           vLimit (V.length $ listElements $ sd ^. sdValues)
                                  (renderList renderValue False $ sd ^. sdValues)


-- handleSearchDialogEvent :: Event -> SearchDialog n -> EventM n (SearchDialog n)
-- handleSearchDialogEvent = traverseOf sdDialog . handleDialogEvent


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
       borderWithLabel (withAttr titleAttr . txt $ title s) $
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
    w <- availWidth <$> getContext
    let v = min (V.length $ listElements $ head $ tv ^. tvColumns) (h-4)
        ws = allocateWidths curF w $ tv ^. tvColWidths
    render ( vLimit 1 (hBox $ withWidths (\(i, t) ->
                                              if i == curF
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
                       borderWithLabel (withAttr titleAttr $ txt lbl) $
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
          , appStartEvent = return ()
          , appAttrMap = const myAttrMap
          }


listKeys :: [Key]
listKeys = [KDown, KUp, KPageUp, KPageDown, KHome, KEnd, KLeft, KRight]


handleEvent :: BrickEvent Name EventType -> EventM Name State ()
handleEvent (VtyEvent (EvKey k ms)) = handleKey k ms
handleEvent _ = return ()

handleKey :: Key -> [Modifier] -> EventM Name State ()
handleKey k m = use sSearch
                >>= (\case
                        Just _ -> handleKeySearch k m
                        Nothing -> handleKeyStandard k m)

handleKeyStandard :: Key -> [Modifier] -> EventM Name State ()
handleKeyStandard (KChar 'q') [MCtrl] = halt
handleKeyStandard (KChar 'f') [MCtrl] = activateSearch
handleKeyStandard (KChar 't') [] = toggleTable
handleKeyStandard (KChar 'z') [] = toggleZoom
handleKeyStandard k [] | k `elem` listKeys = moveLists (EvKey k [])
handleKeyStandard _ _ = return ()


handleKeySearch :: Key -> [Modifier] -> EventM Name State ()
handleKeySearch k [] | k `elem` listKeys = moveSearchList (EvKey k [])
handleKeySearch KEnter [] = moveToSelected
handleKeySearch KEsc [] = deactivateSearch
handleKeySearch k ms = handleInSearchDialog (EvKey k ms)


backward :: EventM Name State ()
backward = uses sIndex (subtract 1) >>= modify . moveTo


forward :: EventM Name State ()
forward = uses sIndex (+ 1) >>= modify . moveTo

toggleZoom :: EventM Name State ()
toggleZoom = use sZoom
             >>= (\case
                     Nothing -> modify zoom
                     Just _ -> modify unZoom)


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


activateSearch :: EventM Name State ()
activateSearch = do
   (index, tle) <- uses (sRowViewer . rvFieldNames)
                        (fromMaybe (0, "") . listSelectedElement)
   vs <- uses sRowStore (fieldValues (fromIntegral index))
   sSearch .= Just (searchDialog SearchList maxWidth tle vs)


deactivateSearch :: EventM Name State ()
deactivateSearch = sSearch .= Nothing


toggleTable :: EventM Name State ()
toggleTable = sIsTable %= not

moveLists :: Event -> EventM Name State ()
moveLists e = use sIsTable >>= (\case
                   True -> moveListsTables e
                   False -> moveListsRows e)

moveListsRows :: Event -> EventM Name State ()
moveListsRows (EvKey KPageUp []) = backward
moveListsRows (EvKey KPageDown []) = forward
moveListsRows e = do
    B.zoom (sRowViewer . rvLists) $ handleListEvent e
    cf <- uses (sRowViewer . rvFieldNames) (fromMaybe 0 . listSelected)
    sCurrentField .= cf
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


moveSearchList :: Event -> EventM Name State ()
moveSearchList e = B.zoom (sSearch . _Just . sdValues) $ handleListEvent e


moveToSelected :: EventM Name State ()
moveToSelected = use sSearch >>= maybe (return ()) (\ss -> do
                     case dialogSelection (ss ^. sdDialog) of
                         Just (DButton OkButton, ()) -> do
                                            deactivateSearch
                                            case listSelectedElement (ss ^. sdValues) of
                                               Nothing -> return ()
                                               Just (_, t) -> do
                                                   s <- get
                                                   let pos = nextPos (fromIntegral $ s ^. sCurrentField) t (s ^. sIndex) (s ^. sRowStore)
                                                   modify $ moveTo pos
                         Just (DButton CancelButton, ()) -> deactivateSearch
                         _ -> return ()
                 )


handleInSearchDialog :: Event -> EventM Name State ()
handleInSearchDialog ev = B.zoom (sSearch . _Just . sdDialog) $ handleDialogEvent ev


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

