{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TUI (
  startTUI
) where

import Brick hiding (getName)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Brick.Widgets.List
import Brick.Widgets.Table
import Data.Maybe(isJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Graphics.Vty.Attributes(defAttr, bold, reverseVideo, withStyle)
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl))

import Model.RowStore
import Graphics.Vty (imageWidth, imageHeight, translate)

data State = State { sRowStore :: RowStore
                   , sIndex :: Int
                   , sCurrentField :: Int
                   , sFieldList :: List Name Text
                   , sFieldWidth :: Int
                   , sValueList :: List Name Text
                   , sZoom :: Maybe Zoom
                   , sSearch :: Maybe (SearchDialog Name)
                   , sTable :: Table Name
                   , sIsTable :: Bool
                   }


type Zoom = (Text, Text)

data SearchDialog n = SearchDialog { sdTitle :: Text
                                   , sdValues :: List n Text
                                   , sdDialog :: Dialog DialogButton
                                   }


data DialogButton = OkButton | CancelButton


searchDialog :: n -> Int -> Text -> [Text] -> SearchDialog n
searchDialog n w ttle values = SearchDialog ttle
                                           (list n (V.fromList values) 1)
                                           (dialog (Just $ T.unpack ttle)
                                                   (Just (0, [ ("OK", OkButton)
                                                             , ("Cancel", CancelButton)]))
                                                   w
                                           )


renderSearchDialog :: SearchDialog Name -> Widget Name
renderSearchDialog SearchDialog {..} = renderDialog sdDialog $
                                           vLimit (V.length $ listElements sdValues)
                                                  (renderList renderValue False sdValues)


handleSearchDialogEvent :: Event -> SearchDialog n -> EventM n (SearchDialog n)
handleSearchDialogEvent ev sd@SearchDialog{..} = do
    d' <- handleDialogEvent ev sdDialog
    return sd { sdDialog = d' }

initialState :: RowStore -> State
initialState rst = State { sRowStore = rst
                         , sIndex = 0
                         , sCurrentField = 0
                         , sFieldList = listMoveTo 0 fl
                         , sFieldWidth = min 40 (V.maximum . V.map T.length $ listElements fl)
                         , sValueList = valueList 0 rst
                         , sZoom = Nothing
                         , sSearch = Nothing
                         , sTable = buildTable rst
                         , sIsTable = False
                         }
                    where fl = fieldList rst


fieldList :: RowStore -> List Name Text
fieldList rst = list FieldList (V.fromList $ fnames rst) 1


valueList :: Int -> RowStore -> List Name Text
valueList pos rst = list ValueList (V.fromList $ map toString $ row pos rst) 1


buildTable :: RowStore -> Table Name
buildTable rst = table $ case names rst of
    Nothing -> rws
    Just ns -> map (withAttr "title" . myTxt) ns : rws
  where rws = map (map $ myTxt . toString) (rows rst)


type EventType = ()


data Name = FieldList | ValueList | SearchList deriving (Eq, Ord, Show)

draw :: State -> [Widget Name]
draw s = [
    renderFront s,
    if sIsTable s
    then renderAsTable s
    else renderRow s
   ]

renderRow :: State -> Widget Name
renderRow s = joinBorders $ center $
       borderWithLabel (txt $ title s) $
       renderFields s
       <=>
       hBorder
       <=>
       hCenter (txt "Enter: zoom field, t: table view, C-f: find, C-q: exit")


renderAsTable :: State -> Widget Name
renderAsTable = renderTable . sTable

tshow :: Int -> Text
tshow = T.pack . show


title :: State -> Text
title State{..} = T.concat [ getName sRowStore
                           , " (", tshow $ sIndex + 1, "/"
                           , tshow $ size sRowStore, ")"
                           ]


renderFields :: State -> Widget Name
renderFields State {..} = Widget Greedy Fixed $ do
    h <- availHeight <$> getContext
    let v = min (V.length $ listElements sFieldList) (h-2)
    render $ vLimit v $ hBox [
                hLimit sFieldWidth (renderList renderName False sFieldList)
                , vBorder
                , renderList renderValue False sValueList
              ]


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
          t' | l == 0 = " "
             | l <= w = t
             | otherwise = T.take (w-3) t <> "..."
      render $ txt t'


renderFront :: State -> Widget Name
renderFront s@State {..}
  | isJust sSearch = renderSearch s
  | isJust sZoom = renderZoom s
  | otherwise = emptyWidget


renderZoom :: State -> Widget Name
renderZoom State {..} = case sZoom of
    Nothing -> emptyWidget
    Just (lbl, t) -> myCenter $ joinBorders $
                       hLimitPercent 95 $
                       borderWithLabel (txt lbl) $
                         txtWrap (if T.null t
                                  then " "
                                  else t)
                         <=>
                         hBorder
                         <=>
                         hCenter (txt "Esc to exit zoom")


renderSearch :: State -> Widget Name
renderSearch = maybe emptyWidget renderSearchDialog . sSearch


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
listKeys = [KDown, KUp, KPageUp, KPageDown, KHome, KEnd]


handleEvent :: State -> BrickEvent Name EventType -> EventM Name (Next State)
handleEvent s (VtyEvent (EvKey k ms)) = handleKey s k ms
handleEvent s _ = continue s

handleKey :: State -> Key -> [Modifier] -> EventM Name (Next State)
handleKey s@State {..} k m
            | isJust sSearch = handleKeySearch k m s
            | otherwise = handleKeyStandard k m s

handleKeyStandard :: Key -> [Modifier] -> State -> EventM Name (Next State)
handleKeyStandard (KChar 'q') [MCtrl] = halt
handleKeyStandard KPageUp [] = backward
handleKeyStandard KPageDown [] = forward
handleKeyStandard KEnter [] = zoom
handleKeyStandard KEsc [] = unZoom
handleKeyStandard (KChar 'f') [MCtrl] = activateSearch
handleKeyStandard (KChar 't') [] = toggleTable
handleKeyStandard k [] | k `elem` listKeys = moveLists (EvKey k [])
handleKeyStandard _ _ = continue


handleKeySearch :: Key -> [Modifier] -> State -> EventM Name (Next State)
handleKeySearch k [] | k `elem` listKeys = moveSearchList (EvKey k [])
handleKeySearch KEnter [] = moveToSelected
handleKeySearch KEsc [] = deactivateSearch
handleKeySearch k ms = handleInSearchDialog (EvKey k ms)


backward :: State -> EventM Name (Next State)
backward s@State {..} = updateZoom $ moveTo (sIndex - 1) s


forward :: State -> EventM Name (Next State)
forward s@State {..} = updateZoom $ moveTo (sIndex + 1) s


updateZoom :: State -> EventM Name (Next State)
updateZoom s@State {..} = case sZoom of
    Nothing -> continue s
    Just _ -> zoom s


unZoom :: State -> EventM Name (Next State)
unZoom s = continue s { sZoom = Nothing }


zoom :: State -> EventM Name (Next State)
zoom s@State {..} = do
  let z = do
             lbl <- snd <$> listSelectedElement sFieldList
             t <- snd <$> listSelectedElement sValueList
             return (lbl, t)
  continue s { sZoom = z }


activateSearch :: State -> EventM Name (Next State)
activateSearch s@State{..} = do
  let (index, tle) = fromMaybe (0, "") $ do
                        listSelectedElement sFieldList
      vs = fieldValues (fromIntegral index) sRowStore
      sd = searchDialog SearchList 40 tle vs
      s' = s { sSearch = Just sd }
  continue s'


deactivateSearch :: State -> EventM Name (Next State)
deactivateSearch s = continue s { sSearch = Nothing }


toggleTable :: State -> EventM Name (Next State)
toggleTable s = continue s { sIsTable = not $ sIsTable s }


moveLists :: Event -> State -> EventM Name (Next State)
moveLists e s@State{..} = do
    fl <- handleListEvent e sFieldList
    vl <- handleListEvent e sValueList
    let cf = fromMaybe 0 $ listSelected fl
    updateZoom s { sCurrentField = cf, sFieldList = fl, sValueList = vl }


moveSearchList :: Event -> State -> EventM Name (Next State)
moveSearchList e s@State{..} = do
    let Just sd@SearchDialog{..} = sSearch
    vs' <- handleListEvent e sdValues
    let sd' = sd { sdValues = vs' }
    continue s { sSearch = Just sd' }


moveToSelected :: State -> EventM Name (Next State)
moveToSelected s@State{..} = do
    let Just SearchDialog{..} = sSearch
    case dialogSelection sdDialog of
        Just OkButton -> case listSelectedElement sdValues of
                             Nothing -> deactivateSearch s
                             Just (_, t) -> let
                                  pos = nextPos (fromIntegral sCurrentField) t sIndex sRowStore
                                in deactivateSearch $ moveTo pos s
        Just CancelButton -> deactivateSearch s
        Nothing -> continue s


handleInSearchDialog :: Event -> State -> EventM Name (Next State)
handleInSearchDialog ev s@State{..} = do
    let Just sd = sSearch
    sd' <- handleSearchDialogEvent ev sd
    continue s { sSearch = Just sd' }


moveTo :: Int -> State -> State
moveTo pos s@State{..}
  | valid = s { sIndex = pos
              , sValueList = vlist
              }
  | otherwise = s
    where valid = 0 <= pos && pos < size sRowStore
          vl = valueList pos sRowStore
          vlist = case listSelected sFieldList of
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

