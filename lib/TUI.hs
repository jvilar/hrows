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
import Graphics.Vty.Attributes(defAttr)
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Modifier(MCtrl))

import Model.RowStore

data State = State { sRowStore :: RowStore
                   , sIndex :: Int
                   , sFieldList :: List Name Text
                   , sFieldWidth :: Int
                   , sValueList :: List Name Text
                   }

initialState :: RowStore -> State
initialState rst = State { sRowStore = rst
                         , sIndex = 0
                         , sFieldList = listMoveTo 0 fl
                         , sFieldWidth = min 40 (V.maximum . V.map T.length $ listElements fl)
                         , sValueList = valueList 0 rst
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
renderFields State {..} = vLimit (V.length $ listElements sFieldList) $
                            hLimit sFieldWidth (renderList renderName False sFieldList)
                            <+>
                            vBorder
                            <+>
                            renderList renderValue False sValueList

renderName :: Bool -> Text -> Widget Name
renderName _ = myTxt

renderValue :: Bool -> Text -> Widget Name
renderValue _ = myTxt

myTxt :: Text -> Widget n
myTxt t = Widget Fixed Fixed 
  $ do
      w <- availWidth <$> getContext
      let l = T.length t
          t' | l == 0 = " "
             | l <= w = t
             | otherwise = T.take (w-3) t <> "..." 
      render $ txt t'


app :: App State EventType Name
app = App { appDraw = draw
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const myAttrMap
          }


handleEvent :: State -> BrickEvent Name EventType -> EventM Name (Next State)
handleEvent s (VtyEvent (EvKey (KChar 'q') [MCtrl])) = halt s
handleEvent s (VtyEvent (EvKey KPageUp [])) = backward s
handleEvent s (VtyEvent (EvKey KPageDown [])) = forward s
handleEvent s _ = continue s

backward :: State -> EventM Name (Next State)
backward s@State {..} = continue $ moveTo (sIndex - 1) s


forward :: State -> EventM Name (Next State)
forward s@State {..} = continue $ moveTo (sIndex + 1) s


moveTo :: Int -> State -> State
moveTo pos s@State{..}
  | valid = s { sIndex = pos
              , sValueList = valueList pos sRowStore
              }
  | otherwise = s
    where valid = 0 <= pos && pos < size sRowStore


myAttrMap :: AttrMap
myAttrMap = attrMap defAttr []


startTUI :: RowStore -> IO ()
startTUI rst = do
  finalState <- defaultMain app (initialState rst)
  return ()

