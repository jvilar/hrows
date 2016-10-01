{-# LANGUAGE OverloadedStrings #-}

module GUI.Update (
            -- *Types
            GUICommand
            -- *Functions
            , updateGUI
) where

import Control.Concurrent.Chan(Chan, writeChan)
import Control.Monad(forM, forM_, unless, when)
import Control.Monad.IO.Class(liftIO)
import Data.IORef(readIORef, writeIORef)
import Data.Maybe(catMaybes, fromJust, isJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums(Align(..))

import Field
import GUI.Command
import GUI.Control
import GUI.Iteration
import Presenter.Input

updateGUI :: GUICommand -> GUIControl -> IO ()
updateGUI (ShowPosition pos size) = updatePosition pos size
updateGUI (ShowRow row) = updateRow row
updateGUI (ShowFieldState c s) = showFieldState c s
updateGUI (ShowNames names) = updateNames names
updateGUI (ShowIteration iter) = showIteration iter
updateGUI DisableTextViews = disableTextViews

updatePosition :: Int -> Int -> GUIControl -> IO ()
updatePosition pos size control = do
    widgetSetSensitive (beginButton control) $ pos > 1
    widgetSetSensitive (leftButton control) $ pos > 1
    widgetSetSensitive (endButton control) $ pos < size
    widgetSetSensitive (rightButton control) $ pos < size

    labelSetText (positionLabel control) $ show pos ++ "/" ++ show size

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

red :: Color
red = Color 65535 0 0

updateRow :: [(String, FieldState)] -> GUIControl -> IO ()
updateRow row control = do
  let grid = rowsGrid control

  adjustRows (length row) control

  forM_ (enumerate row) $ \(c, (field, s)) -> do
                             textView <- recoverColumnView c control
                             changeBackground s textView
                             set textView [ textViewEditable := True
                                          , widgetCanFocus := True
                                          , widgetState := StateNormal
                                          ]
                             buffer <- textViewGetBuffer textView
                             textBufferSetText buffer field
  widgetShowAll grid

showFieldState :: Int -> FieldState -> GUIControl -> IO ()
showFieldState c s control = recoverColumnView c control >>= changeBackground s

changeBackground :: FieldState -> TextView -> IO ()
changeBackground s textView =
    widgetOverrideBackgroundColor textView StateNormal $ case s of
                                                             NormalFieldState ->  Nothing
                                                             ErrorFieldState -> Just red

recoverColumnView :: Int -> GUIControl -> IO TextView
recoverColumnView c control = do
    Just tv <- gridGetChildAt (rowsGrid control) 1 c
    return $ castToTextView tv

disableTextViews :: GUIControl -> IO ()
disableTextViews control = do
  let grid = rowsGrid control
  nrows <- readIORef $ currentRows control
  forM_ [0..nrows-1] $ \r -> do
                               textView <- recoverColumnView r control
                               set textView [ textViewEditable := False
                                            , widgetCanFocus := False
                                            , widgetState := StateInsensitive
                                            ]

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
                              , textViewEditable := False
                              , widgetState := StateInsensitive
                              , widgetCanFocus := False
                              , widgetHExpand := True
                              ]
                 buffer <- textViewGetBuffer textView
                 textView `on` keyReleaseEvent $ liftIO $ do
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

showIteration :: Iteration -> GUIControl -> IO ()
showIteration AskReadFile = askReadFile
showIteration AskWriteFile = askWriteFile
showIteration AskCreateField = askCreateField
showIteration AskDeleteField = askDeleteField
showIteration (DisplayMessage m) = displayMessage m
showIteration ConfirmExit = confirmExit

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
                          ("¿Seguro que quieres salir?" :: String)
  r <- dialogRun dlg
  when (r == ResponseYes) $ do
                        sendInput control DoExit
                        mainQuit
  widgetDestroy dlg

askCreateField :: GUIControl -> IO ()
askCreateField control = do
    dlg <- dialogNew
    set dlg [ windowTransientFor := mainWindow control
            , windowModal := True
            ]
    dialogAddButton dlg
                    ("Crear" :: String)
                    ResponseOk
    dialogAddButton dlg
                    ("Cancelar" :: String)
                    ResponseCancel
    content <- castToContainer <$> dialogGetContentArea dlg
    labelNew (Just ("Crear Campos" :: String)) >>= containerAdd content

    grid <- gridNew
    addLabel grid "Nombre" 0 0
    addLabel grid "Tipo" 1 0
    entries <- forM [1..5] $ \row -> (,)
                                    <$> addEntry grid 0 row
                                    <*> addComboBox grid ["Cadena", "Entero", "Flotante"] 1 row

    actionArea <- castToContainer <$> dialogGetActionArea dlg
    containerAdd actionArea grid

    widgetShowAll dlg
    r <- dialogRun dlg

    when (r == ResponseOk) $ do
        fields <- catMaybes <$> (forM entries $ \(entry, cbox) -> do
                                        name <- entryGetText entry
                                        if null name
                                            then return Nothing
                                            else do
                                               mtext <- comboBoxGetActiveText cbox
                                               return $ do
                                                   text <- mtext
                                                   t <- lookup text [("Cadena", TypeString)
                                                                    ,("Entero", TypeInt)
                                                                    ,("Flotante", TypeDouble)
                                                                    ]
                                                   return (name, t)
                                )
        unless (null fields) $ sendInput control $ NewFields fields
    widgetDestroy dlg

addLabel :: Grid -> String -> Int -> Int -> IO ()
addLabel grid text left top = do
    lbl <- labelNew $ Just text
    gridAttach grid lbl left top 1 1

addEntry :: Grid -> Int -> Int -> IO Entry
addEntry grid left top = do
    entry <- entryNew
    gridAttach grid entry left top 1 1
    return entry

addComboBox :: Grid -> [ComboBoxText] -> Int -> Int -> IO ComboBox
addComboBox grid options left top = do
    cbox <- comboBoxNewWithEntry
    renderer <- cellRendererComboNew
    comboBoxSetModelText cbox
    cellLayoutPackStart cbox renderer True
    forM_ options $ comboBoxAppendText cbox
    set renderer [ cellComboHasEntry := False ]
    gridAttach grid cbox left top 1 1
    return cbox

askDeleteField :: GUIControl -> IO ()
askDeleteField = unimplemented "Borrar campo"

unimplemented :: String -> GUIControl -> IO ()
unimplemented func control = sendInput control . MessageDialog . ErrorMessage $ "Función " ++ func ++ " no implementada"
