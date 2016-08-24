module GUI (
            -- *Functions
            makeGUI
           , updateGUI
) where

import Control.Concurrent.Chan(Chan, writeChan)
import Control.Monad(forM_, void)
import Control.Monad.IO.Class(liftIO)
import Graphics.UI.Gtk

import DisplayInfo
import Input

data GUIControl = GUIControl { positionLabel :: Label
                             , rowsGrid :: Grid
                             }

makeGUI :: Chan Input -> IO GUIControl
makeGUI inputChan = do
  initGUI

  builder <- builderNew
  builderAddFromFile builder "hrows.glade"

  prepareMainWindow builder
  prepareMovementButtons builder inputChan
  prepareQuitButton builder

  prepareControl builder

prepareControl :: Builder -> IO GUIControl
prepareControl builder = do
  lbl <- builderGetObject builder castToLabel "positionLabel"
  grid <- builderGetObject builder castToGrid "rowsGrid"
  return $ GUIControl lbl grid


prepareMainWindow :: Builder -> IO ()
prepareMainWindow builder = do
  window <- builderGetObject builder castToWindow "mainWindow"
  void (window `on` deleteEvent $ do
        liftIO mainQuit
        return False)
  widgetShowAll window

prepareMovementButtons :: Builder -> Chan Input -> IO()
prepareMovementButtons builder inputChan =
  forM_ [ ("beginButton", MoveBegin)
        , ("endButton", MoveEnd)
        , ("leftButton", MovePrevious)
        , ("rightButton", MoveNext)
        ] $ \(name, input) -> do
    btn <- builderGetObject builder castToButton name
    btn `on` buttonActivated $ (writeChan inputChan (InputMove input) >> print input)

prepareQuitButton :: Builder -> IO ()
prepareQuitButton builder = do
  btn <- builderGetObject builder castToButton "quitButton"
  void (btn `on` buttonActivated $ mainQuit)

updateGUI :: GUIControl -> DisplayInfo -> IO ()
updateGUI control dinfo = do
  updatePosition control dinfo
  updateRows control dinfo

updatePosition :: GUIControl -> DisplayInfo -> IO ()
updatePosition control dinfo = labelSetText (positionLabel control) positionText
    where positionText = show (position dinfo + 1) ++ "/" ++ show (modelSize dinfo)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

updateRows :: GUIControl -> DisplayInfo -> IO ()
updateRows control dinfo = do
  let grid = rowsGrid control
  children <- containerGetChildren grid
  mapM_ widgetDestroy children
  forM_ (enumerate $ zip (fieldNames dinfo) (fields dinfo)) $ \(row, (name, field)) -> do
                             lbl <- labelNew (Just name)
                             gridAttach grid lbl 0 row 1 1
                             entry <- entryNew
                             set entry [ entryText := field
                                       , widgetCanFocus := True
                                       , widgetHExpand := True
                                       ]
                             gridAttach grid entry 1 row 1 1
  widgetShowAll grid
