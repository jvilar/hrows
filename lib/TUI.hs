{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI (
  startTUI
) where


import Brick ( EventM,
      Widget(Widget, render),
      Context(availHeight, availWidth),
      Size(Fixed),
      Result(image),
      App(..),
      CursorLocation(cursorLocationName),
      txt,
      getContext,
      getVtyHandle,
      customMain )
import Brick.BChan qualified as B
import Brick.Widgets.Core qualified as BC
import Control.Concurrent (threadDelay, forkIO)
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List(find)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty (imageWidth, imageHeight, translate, Vty (outputIface), supportsMode)
import Graphics.Vty.Config qualified as Vty
import Graphics.Vty.CrossPlatform qualified as Vty
import Graphics.Vty.Output (Mode(Mouse), setMode)

import Model.Expression.RecursionSchemas ( cata )
import Model.RowStore ( RowStore, getName, changed, size )
import Model.SourceInfo ( SourceInfo )

import TUI.Base ( Name, myAttrMap, veEditor )
import TUI.Events ( EventType, BackupEvent(..), handleEvent )
import TUI.Level ( Level(..), activeEditor, renderDialogLevel, renderZoomLevel, renderBackLevel )
import TUI.State ( State, sIndex, sInterface, sLog, sRowStore, initialState )

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

showSelectedCursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
showSelectedCursor s cs = do
    ed <- s ^. sInterface . activeEditor
    find ((== Just (BC.getName $ ed ^. veEditor)) . cursorLocationName) cs

backupLoop :: B.BChan EventType -> IO ()
backupLoop chan = do
  threadDelay $ 60 * 1000000
  B.writeBChan chan BackupEvent
  backupLoop chan

startTUI :: RowStore -> Maybe (SourceInfo, [SourceInfo]) -> IO ()
startTUI rst msi = do
  eventChan <- B.newBChan 10
  _ <- forkIO $ backupLoop eventChan
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty (Just eventChan) app (initialState rst msi)
  return ()
