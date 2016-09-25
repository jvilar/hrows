{-# LANGUAGE OverloadedStrings #-}

module GUI.Update (
            -- *Types
            GUICommand
            -- *Functions
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

import GUI.Command
import GUI.Control
import Input
import Iteration
import Model

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
askReadFile = askFile FileChooserActionOpen LoadFileFromName

askWriteFile :: GUIControl -> IO ()
askWriteFile = askFile FileChooserActionSave WriteFileFromName

askFile :: IsInput t => FileChooserAction -> (String -> t) -> GUIControl -> IO ()
askFile action input control = do
    dlg <- fileChooserDialogNew (Just "Escribir fichero" :: Maybe String)
                                (Just $ mainWindow control)
                                action
                                [("OK", ResponseOk), ("Cancelar", ResponseCancel)]
    r <- dialogRun dlg
    when (r == ResponseOk) $ do
            file <- fileChooserGetFilename dlg
            when (isJust file) $
                 sendInput control $ input (fromJust file)
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
