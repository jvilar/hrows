{-# LANGUAGE OverloadedStrings #-}

module GUI (
            -- *Types
            GUIControl
            , GUICommand
            -- *Functions
            , makeGUI
            , updateGUI
) where

import Control.Concurrent.Chan(Chan, writeChan)
import Control.Monad(forM_, void, when)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(ask, ReaderT, runReaderT)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Maybe(isJust, fromJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums(Align(..))

import Paths_hrows(getDataFileName)

import GUICommand
import Input
import Iteration
import Model

data GUIControl = GUIControl { mainWindow :: Window
                             , positionLabel :: Label
                             , rowsGrid :: Grid
                             , currentRows :: IORef Int
                             , inputChan :: Chan Input
                             }

sendInput :: IsInput cmd => GUIControl -> cmd -> IO ()
sendInput control = writeChan (inputChan control) . toInput

makeGUI :: Chan Input -> IO GUIControl
makeGUI iChan = do
  initGUI

  builder <- builderNew
  gladefn <- getDataFileName "hrows.glade"
  builderAddFromFile builder gladefn

  runReaderT (do
                control <- prepareControl iChan

                liftIO $ prepareMainWindow control
                prepareMovementButtons iChan
                prepareQuitButton control
                prepareFileMenu control
                return control
             ) builder


type BuildMonad = ReaderT Builder IO

getObject :: GObjectClass obj => (GObject -> obj) -> String -> BuildMonad obj
getObject cast s = do
    builder <- ask
    liftIO $ builderGetObject builder cast s

ioVoid :: IO a -> BuildMonad ()
ioVoid = liftIO . void

buttonAction :: String -> IO () -> BuildMonad ()
buttonAction name action = do
    btn <- getObject castToButton name
    ioVoid (btn `on` buttonActivated $ action)

menuItemAction :: String -> IO () -> BuildMonad ()
menuItemAction name action = do
    itm <- getObject castToMenuItem name
    ioVoid (itm `on` menuItemActivated $ action)

prepareControl :: Chan Input -> BuildMonad GUIControl
prepareControl iChan = do
  lbl <- getObject castToLabel "positionLabel"
  grid <- getObject castToGrid "rowsGrid"
  window <- getObject castToWindow "mainWindow"
  rows <- liftIO $ newIORef 0
  return GUIControl { mainWindow = window
                    , positionLabel = lbl
                    , rowsGrid = grid
                    , currentRows = rows
                    , inputChan = iChan
                    }

global_keys = [ (("Page_Down", []), toInput MoveNext)
              , (("Page_Up", []), toInput MovePrevious)
              , (("q", [Control]), toInput ExitProgram)
              ]

prepareMainWindow :: GUIControl -> IO ()
prepareMainWindow control = do
  let window = mainWindow control
  void (window `on` deleteEvent $ do
        liftIO $ sendInput control ExitProgram
        return False)
  void (window `on` keyPressEvent $ do
          name <- eventKeyName
          mods <- eventModifier
          -- showEvent
          let cmd = lookup (name, mods) global_keys
          liftIO $ print cmd
          maybe (return False)
                (\c -> liftIO $ sendInput control c >> return True)
                cmd
       )
  widgetShowAll window

showEvent = do
  name <- eventKeyName
  liftIO $ putStrLn $ "Key name: " ++ show name
  mods <- eventModifier
  liftIO $ putStrLn $ "Modifiers: " ++ show mods

prepareMovementButtons :: Chan Input -> BuildMonad ()
prepareMovementButtons iChan =
  forM_ [ ("beginButton", MoveBegin)
        , ("endButton", MoveEnd)
        , ("leftButton", MovePrevious)
        , ("rightButton", MoveNext)
        ] $ \(name, input) ->
    buttonAction name $ writeChan iChan (InputMove input)

prepareQuitButton :: GUIControl -> BuildMonad ()
prepareQuitButton control = buttonAction "quitButton" $ sendInput control ExitProgram

prepareFileMenu :: GUIControl -> BuildMonad ()
prepareFileMenu control = mapM_ (\(n, cmd) -> menuItemAction n $ sendInput control cmd)
                            [("openMenuItem", toInput LoadFileDialog)
                            ,("saveMenuItem",  toInput WriteFile)
                            ,("saveAsMenuItem", toInput SaveAsFileDialog)
                            ,("quitMenuItem", toInput ExitProgram)
                            ]

updateGUI :: GUICommand -> GUIControl -> IO ()
updateGUI (ShowPosition pos size) = updatePosition pos size
updateGUI (ShowRow row) = updateRow row
updateGUI (ShowNames names) = updateNames names
updateGUI (ShowIteration AskReadFile) = askReadFile
updateGUI (ShowIteration AskWriteFile) = askWriteFile
updateGUI (ShowIteration (DisplayMessage m)) = displayMessage m
updateGUI (ShowIteration ConfirmExit) = confirmExit

updatePosition :: Int -> Int -> GUIControl -> IO ()
updatePosition pos size control = labelSetText (positionLabel control) positionText
    where positionText = let
              p = if size == 0
                    then 0
                    else pos + 1
              in show pos ++ "/" ++ show size

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

updateRow :: [String] -> GUIControl -> IO ()
updateRow row control = do
  let grid = rowsGrid control

  adjustRows (length row) control

  forM_ (enumerate row) $ \(r, field) -> do
                             Just tv <- gridGetChildAt grid 1 r
                             buffer <- textViewGetBuffer $ castToTextView tv
                             textBufferSetText buffer field
  widgetShowAll grid

updateNames :: [String] -> GUIControl -> IO ()
updateNames names control = do
  let grid = rowsGrid control
  adjustRows (length names) control

  forM_ (enumerate names) $ \(r, name) -> do
                             Just lbl <- gridGetChildAt grid 0 r
                             labelSetText (castToLabel lbl) name
  widgetShowAll grid

adjustRows :: Int -> GUIControl -> IO ()
adjustRows nrows control = do
  let grid = rowsGrid control
  current <- readIORef $ currentRows control
  case compare current nrows of
    LT -> addRows grid [current .. nrows - 1] (inputChan control)
    EQ -> return ()
    GT -> deleteRows grid [nrows .. current - 1]
  writeIORef (currentRows control) nrows

addRows :: Grid -> [Int] -> Chan Input -> IO ()
addRows grid rows chan = forM_ rows $ \r -> do
                 lbl <- labelNew $ Just ("" :: String)
                 widgetSetHAlign lbl AlignStart
                 gridAttach grid lbl 0 r 1 1
                 textView <- textViewNew
                 set textView [ textViewWrapMode := WrapWord
                              , textViewAcceptsTab := False
                              , widgetCanFocus := True
                              , widgetHExpand := True
                              ]
                 buffer <- textViewGetBuffer textView
                 textView `on` focusOutEvent $ liftIO $ do
                     begin <- textBufferGetStartIter buffer
                     end <- textBufferGetEndIter buffer
                     text <- textBufferGetText buffer begin end False
                     writeChan chan (toInput $ UpdateField r (toField (text::String)))
                     return False
                 gridAttach grid textView 1 r 1 1

deleteRows :: Grid -> [Int] -> IO ()
deleteRows grid rows = forM_ rows $ \r ->
                         forM_ [0, 1] $ \c -> do
                             Just w <- gridGetChildAt grid c r
                             widgetDestroy w

displayMessage :: Message -> GUIControl -> IO ()
displayMessage (ErrorMessage m) = noResponseMessage m MessageError
displayMessage (WarningMessage m) = noResponseMessage m MessageWarning
displayMessage (InformationMessage m) = noResponseMessage m MessageWarning
displayMessage (QuestionMessage m) = undefined

noResponseMessage :: String -> MessageType -> GUIControl -> IO ()
noResponseMessage m mtype control = do
    dlg <- messageDialogNew (Just $ mainWindow control)
                            []
                            mtype
                            ButtonsOk
                            m
    dialogRun dlg
    widgetDestroy dlg

askReadFile :: GUIControl -> IO ()
askReadFile control = do
    dlg <- fileChooserDialogNew (Just "Abrir fichero" :: Maybe String)
                                (Just $ mainWindow control)
                                FileChooserActionOpen
                                [("OK", ResponseOk), ("Cancelar", ResponseCancel)]
    r <- dialogRun dlg
    when (r == ResponseOk) $ do
            file <- fileChooserGetFilename dlg
            when (isJust file) $
                 sendInput control $ LoadFileFromName (fromJust file)
    widgetDestroy dlg

askWriteFile :: GUIControl -> IO ()
askWriteFile control = do
    dlg <- fileChooserDialogNew (Just "Escribir fichero" :: Maybe String)
                                (Just $ mainWindow control)
                                FileChooserActionSave
                                [("OK", ResponseOk), ("Cancelar", ResponseCancel)]
    r <- dialogRun dlg
    when (r == ResponseOk) $ do
            file <- fileChooserGetFilename dlg
            when (isJust file) $
                 sendInput control $ WriteFileFromName (fromJust file)
    widgetDestroy dlg

confirmExit :: GUIControl -> IO ()
confirmExit control = do
  dlg <- messageDialogNew (Just $ mainWindow control)
                          [DialogModal]
                          MessageQuestion
                          ButtonsYesNo
                          ("Seguro que quieres salir" :: String)
  r <- dialogRun dlg
  when (r == ResponseYes) $ do
                        sendInput control DoExit
                        mainQuit
  widgetDestroy dlg
