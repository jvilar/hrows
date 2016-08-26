module GUI (
            -- *Types
            GUIControl
            -- *Functions
            , makeGUI
           , updateGUI
) where

import Control.Concurrent.Chan(Chan, writeChan)
import Control.Monad(forM_, void)
import Control.Monad.IO.Class(liftIO)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums(Align(..))

import DisplayInfo
import Input
import Model

data GUIControl = GUIControl { positionLabel :: Label
                             , rowsGrid :: Grid
                             , currentRows :: IORef Int
                             , inputChan :: Chan Input
                             }

makeGUI :: Chan Input -> IO GUIControl
makeGUI iChan = do
  initGUI

  builder <- builderNew
  builderAddFromFile builder "hrows.glade"

  prepareMainWindow builder
  prepareMovementButtons builder iChan
  prepareQuitButton builder
  prepareFileMenu builder

  prepareControl builder iChan

prepareControl :: Builder -> Chan Input -> IO GUIControl
prepareControl builder iChan = do
  lbl <- builderGetObject builder castToLabel "positionLabel"
  grid <- builderGetObject builder castToGrid "rowsGrid"
  rows <- newIORef 0
  return $ GUIControl lbl grid rows iChan


prepareMainWindow :: Builder -> IO ()
prepareMainWindow builder = do
  window <- builderGetObject builder castToWindow "mainWindow"
  void (window `on` deleteEvent $ do
        liftIO mainQuit
        return False)
  widgetShowAll window

prepareMovementButtons :: Builder -> Chan Input -> IO()
prepareMovementButtons builder iChan =
  forM_ [ ("beginButton", MoveBegin)
        , ("endButton", MoveEnd)
        , ("leftButton", MovePrevious)
        , ("rightButton", MoveNext)
        ] $ \(name, input) -> do
    btn <- builderGetObject builder castToButton name
    btn `on` buttonActivated $ writeChan iChan (InputMove input)

prepareQuitButton :: Builder -> IO ()
prepareQuitButton builder = do
  btn <- builderGetObject builder castToButton "quitButton"
  void (btn `on` buttonActivated $ mainQuit)

prepareFileMenu :: Builder -> IO ()
prepareFileMenu builder = do
  itm <- builderGetObject builder castToMenuItem "quitMenuItem"
  void (itm `on` menuItemActivated $ mainQuit)

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

  adjustRows control (length $ fields dinfo)

  forM_ (enumerate $ zip (fieldNames dinfo) (fields dinfo)) $ \(r, (name, field)) -> do
                             Just lbl <- gridGetChildAt grid 0 r
                             labelSetText (castToLabel lbl) name
                             Just entry <- gridGetChildAt grid 1 r
                             set (castToEntry entry) [ entryText := field ]
  widgetShowAll grid

adjustRows :: GUIControl -> Int -> IO ()
adjustRows control nrows = do
  let grid = rowsGrid control
  current <- readIORef $ currentRows control
  case compare current nrows of
    LT -> addRows grid [current .. nrows - 1] (inputChan control)
    EQ -> return ()
    GT -> deleteRows grid [nrows .. current - 1]
  writeIORef (currentRows control) nrows

addRows :: Grid -> [Int] -> Chan Input -> IO ()
addRows grid rows chan = forM_ rows $ \r -> do
                 lbl <- labelNew $ Just ""
                 widgetSetHAlign lbl AlignStart
                 gridAttach grid lbl 0 r 1 1
                 entry <- entryNew
                 set entry [ entryText := ""
                           , widgetCanFocus := True
                           , widgetHExpand := True
                           ]
                 entry `on` focusOutEvent $ liftIO $ do
                   text <- get entry entryText
                   writeChan chan (InputUpdate $ UpdateField r (toField (text::String)))
                   return False
                 gridAttach grid entry 1 r 1 1

deleteRows :: Grid -> [Int] -> IO ()
deleteRows grid rows = forM_ rows $ \r ->
                         forM_ [0, 1] $ \c -> do
                             Just w <- gridGetChildAt grid c r
                             widgetDestroy w
