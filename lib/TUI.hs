{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TUI (
  startTUI
) where

import Brick hiding (getName)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Graphics.Vty.Attributes(defAttr, reverseVideo, withStyle)
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl))

import Model.RowStore

data State = State { sRowStore :: RowStore
                   , sIndex :: Int
                   , sFieldList :: List Name Text
                   , sFieldWidth :: Int
                   , sValueList :: List Name Text
                   , sZoom :: Maybe Zoom
                   }


type Zoom = (Text, Text)


initialState :: RowStore -> State
initialState rst = State { sRowStore = rst
                         , sIndex = 0
                         , sFieldList = listMoveTo 0 fl
                         , sFieldWidth = min 40 (V.maximum . V.map T.length $ listElements fl)
                         , sValueList = valueList 0 rst
                         , sZoom = Nothing
                         }
                    where fl = fieldList rst


fieldList :: RowStore -> List Name Text
fieldList rst = list FieldList (V.fromList $ fnames rst) 1


valueList :: Int -> RowStore -> List Name Text
valueList pos rst = list ValueList (V.fromList $ map toString $ row pos rst) 1


type EventType = ()

data Name = FieldList | ValueList deriving (Eq, Ord, Show)

draw :: State -> [Widget Name]
draw s = [
    renderZoom s,
    joinBorders $ center $
       borderWithLabel (txt $ title s) $
       renderFields s
       <=>
       hBorder
       <=>
       hCenter (txt "C-q: exit")
    ]


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
renderName = renderElement


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


renderZoom :: State -> Widget Name
renderZoom State {..} = case sZoom of
    Nothing -> txt ""
    Just (lbl, t) -> borderWithLabel (txt lbl) $ txt t


app :: App State EventType Name
app = App { appDraw = draw
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const myAttrMap
          }


listKeys :: [Key]
listKeys = [KDown, KUp]


handleEvent :: State -> BrickEvent Name EventType -> EventM Name (Next State)
handleEvent s (VtyEvent (EvKey (KChar 'q') [MCtrl])) = halt s
handleEvent s (VtyEvent (EvKey KPageUp [])) = backward s
handleEvent s (VtyEvent (EvKey KPageDown [])) = forward s
handleEvent s (VtyEvent (EvKey KEnter [])) = zoom s
handleEvent s (VtyEvent e@(EvKey k [])) | k `elem` listKeys = moveLists s e
handleEvent s _ = continue s


backward :: State -> EventM Name (Next State)
backward s@State {..} = continue $ moveTo (sIndex - 1) s


forward :: State -> EventM Name (Next State)
forward s@State {..} = continue $ moveTo (sIndex + 1) s


zoom :: State -> EventM Name (Next State)
zoom s@State {..} = do
  let z = do
             lbl <- snd <$> listSelectedElement sFieldList
             t <- snd <$> listSelectedElement sValueList
             return (lbl, t)
  continue s { sZoom = z }


moveLists :: State -> Event -> EventM Name (Next State)
moveLists s@State{..} e = do
    fl <- handleListEvent e sFieldList
    vl <- handleListEvent e sValueList
    continue s { sFieldList = fl, sValueList = vl }


moveTo :: Int -> State -> State
moveTo pos s@State{..}
  | valid = s { sIndex = pos
              , sValueList = valueList pos sRowStore
              }
  | otherwise = s
    where valid = 0 <= pos && pos < size sRowStore


myAttrMap :: AttrMap
myAttrMap = attrMap defAttr [ ("selectedElement", withStyle defAttr reverseVideo) ]


startTUI :: RowStore -> IO ()
startTUI rst = do
  finalState <- defaultMain app (initialState rst)
  return ()

