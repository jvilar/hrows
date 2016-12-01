{-# LANGUAGE OverloadedStrings #-}

module GUI.Update (
            -- *Types
            GUICommand
            -- *Functions
            , updateGUI
) where

import Control.Concurrent.Chan(writeChan)
import Control.Monad(filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class(liftIO)
import Data.IORef(readIORef, writeIORef)
import Data.List(elemIndex)
import Data.Maybe(catMaybes, fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums(Align(..))

import GUI.Command
import GUI.Control
import Model.Field
import Presenter.Input

updateGUI :: GUICommand -> GUIControl -> IO ()
updateGUI (ChangeTitle title) = changeTitle title
updateGUI (ShowPosition pos size) = updatePosition pos size
updateGUI (ShowFields fis) = showFields fis
updateGUI (ShowNames names) = updateNames names
updateGUI (ShowIteration iter) = showIteration iter
updateGUI DisableTextViews = disableTextViews


changeTitle :: String -> GUIControl -> IO ()
changeTitle title control = set (mainWindow control) [ windowTitle := title ]

updatePosition :: Int -> Int -> GUIControl -> IO ()
updatePosition pos size control = do
    widgetSetSensitive (beginButton control) $ pos > 1
    widgetSetSensitive (leftButton control) $ pos > 1
    widgetSetSensitive (endButton control) $ pos < size
    widgetSetSensitive (rightButton control) $ pos < size

    labelSetText (positionLabel control) $ show pos ++ "/" ++ show size

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

errorColor :: Color
errorColor = Color 65535 36864 2560

formulaColor :: Color
formulaColor = Color 53000 53000 53000

emptyColor :: Color
emptyColor = Color 53000 53000 53000

normalColor :: Color
normalColor = Color 65535 65535 65535

showFields :: [FieldInfo] -> GUIControl -> IO ()
showFields fis control = do
  let grid = fieldsGrid control

  forM_ fis $ \fi -> do
                       textView <- recoverColumnView (indexFI fi) control
                       let tooltip = fromMaybe (typeLabel $ typeFI fi) $ formulaFI fi
                       set textView [ textViewEditable := isNothing $ formulaFI fi
                                    , widgetCanFocus := isNothing $ formulaFI fi
                                    , widgetState := StateNormal
                                    , widgetTooltipText := Just tooltip
                                    ]
                       widgetModifyBg textView StateNormal $ if isErrorFI fi
                                                             then errorColor
                                                             else if isJust $ formulaFI fi
                                                                  then formulaColor
                                                                  else normalColor
                       buffer <- textViewGetBuffer textView
                       forM_ (textFI fi) $
                            textBufferSetText buffer
  widgetShowAll grid

recoverColumnView :: Int -> GUIControl -> IO TextView
recoverColumnView c control = do
    Just tv <- gridGetChildAt (fieldsGrid control) 1 c
    return $ castToTextView tv

disableTextViews :: GUIControl -> IO ()
disableTextViews control = do
  nfields <- readIORef $ numberOfFields control
  forM_ [0..nfields-1] $ \f -> do
                                textView <- recoverColumnView f control
                                set textView [ textViewEditable := False
                                             , widgetCanFocus := False
                                             , widgetState := StateInsensitive
                                             ]
                                widgetModifyBg textView StateInsensitive emptyColor

updateNames :: [String] -> GUIControl -> IO ()
updateNames names control = do
  let grid = fieldsGrid control
  adjustTextFields (length names) control

  forM_ (enumerate names) $ \(r, name) -> do
                             Just ebox <- gridGetChildAt grid 0 r
                             containerForeach (castToEventBox ebox) $ \lbl ->
                                 labelSetText (castToLabel lbl) name
  widgetShowAll grid

adjustTextFields :: Int -> GUIControl -> IO ()
adjustTextFields nfields control = do
  let grid = fieldsGrid control
  current <- readIORef $ numberOfFields control
  case compare current nfields of
    LT -> addFields grid [current .. nfields - 1] control
    EQ -> return ()
    GT -> deleteFields grid [nfields .. current - 1]
  writeIORef (numberOfFields control) nfields

addFields :: Grid -> [FieldPos] -> GUIControl -> IO ()
addFields grid fields control = forM_ fields $ \f -> do
                 lbl <- createFieldLabel f control
                 gridAttach grid lbl 0 f 1 1
                 textView <- createFieldTextView f control
                 gridAttach grid textView 1 f 1 1

createFieldLabel :: FieldPos -> GUIControl -> IO EventBox
createFieldLabel f control = do
         lbl <- labelNew $ Just ("" :: String)
         widgetSetHAlign lbl AlignStart
         ebox <- eventBoxNew
         dragSourceSet ebox [Button1] [ActionMove]
         dragSourceSetTargetList ebox (targetList control)
         dragDestSet ebox [DestDefaultAll] [ActionMove]
         dragDestSetTargetList ebox (targetList control)
         ebox `on` dragDataGet $ \_ _ _ -> do
                               ok <- selectionDataSetText (show f)
                               unless ok (liftIO $ dndError control)
         ebox `on` dragDataReceived $ \_ _ _ _ -> do
                               t <- selectionDataGetText
                               liftIO $ case (t :: Maybe String) of
                                          Nothing -> dndError control
                                          Just v -> let
                                                      from = read v
                                                    in if from /= f
                                                       then writeChan (inputChan control) . toInput $ MoveField from f
                                                       else return ()
         containerAdd ebox lbl
         return ebox

dndError :: GUIControl -> IO ()
dndError control = writeChan (inputChan control) . toInput $ MessageDialog (ErrorMessage "Algo está mal en el dnd")

createFieldTextView :: FieldPos -> GUIControl -> IO TextView
createFieldTextView f control = do
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
             writeChan (inputChan control) (toInput $ UpdateField f (toField (text::String)))
             return False
         textView `on` buttonPressEvent $ do
             button <- eventButton
             if button == RightButton
             then liftIO $ do
                      writeIORef (currentField control) f
                      menuPopup (fieldMenu control) Nothing
                      return True
             else return False
         return textView

deleteFields :: Grid -> [FieldPos] -> IO ()
deleteFields grid fields = forM_ fields $ \f ->
                         forM_ [0, 1] $ \c -> do
                             Just w <- gridGetChildAt grid c f
                             widgetDestroy w

showIteration :: Iteration -> GUIControl -> IO ()
showIteration AskReadFile = askReadFile
showIteration AskWriteFile = askWriteFile
showIteration AskCreateField = askCreateField
showIteration (AskDeleteFields fs) = askDeleteField fs
showIteration (DisplayMessage m) = displayMessage m
showIteration (ConfirmExit changed) = confirmExit changed
showIteration (GetFieldFormula fpos flabel ms) = getFieldFormula fpos flabel ms
showIteration (SearchField fpos initial l) = searchField fpos initial l

displayMessage :: Message -> GUIControl -> IO ()
displayMessage (ErrorMessage m) = noResponseMessage m MessageError
displayMessage (WarningMessage m) = noResponseMessage m MessageWarning
displayMessage (InformationMessage m) = noResponseMessage m MessageWarning
displayMessage (QuestionMessage _) = undefined

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
askReadFile = askFile loadFileDialog confFileLoadCheckButton LoadFileFromName

askWriteFile :: GUIControl -> IO ()
askWriteFile = askFile saveAsDialog confFileSaveCheckButton WriteFileFromName


askFile :: IsInput t => (GUIControl -> FileChooserDialog)
                     -> (GUIControl -> CheckButton)
                     -> (String -> Bool -> t)
                     -> GUIControl -> IO ()
askFile dlg btn input control = do
    r <- dialogRun (dlg control)
    widgetHide (dlg control)
    when (r == ResponseOk) $ do
            file <- fileChooserGetFilename (dlg control)
            when (isJust file) $ do
                chk <- toggleButtonGetActive (btn control)
                sendInput control $ input (fromJust file) chk

confirmExit :: Bool -> GUIControl -> IO ()
confirmExit changed control = do
  let msg :: String
      msg = if changed
            then "Ha habido cambios, ¿quieres salir?"
            else "¿Seguro que quieres salir?"
  dlg <- messageDialogNew (Just $ mainWindow control)
                          [DialogModal]
                          MessageQuestion
                          ButtonsYesNo
                          msg
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
                                    <*> addComboBox grid (map snd typeLabels) 1 row

    actionArea <- castToContainer <$> dialogGetActionArea dlg
    containerAdd actionArea grid

    widgetShowAll dlg
    r <- dialogRun dlg

    when (r == ResponseOk) $ do
        fields <- catMaybes <$> forM entries (\(entry, cbox) -> do
                                        name <- entryGetText entry
                                        i <- comboBoxGetActive cbox
                                        return $ if null name || i == -1
                                                 then Nothing
                                                 else Just (name, fst $ typeLabels !! i)
                                )
        unless (null fields) $ sendInput control $ NewFields fields
    widgetDestroy dlg

addLabel :: Grid -> String -> Int -> Int -> IO ()
addLabel grid text left top = do
    lbl <- labelNew $ Just text
    gridAttach grid lbl left top 1 1

addCheckButton :: Grid -> String -> Int -> Int -> IO CheckButton
addCheckButton grid label left top = do
    btn <- checkButtonNewWithLabel label
    gridAttach grid btn left top 1 1
    return btn

addEntry :: Grid -> Int -> Int -> IO Entry
addEntry grid left top = do
    entry <- entryNew
    gridAttach grid entry left top 1 1
    return entry

addComboBox :: Grid -> [ComboBoxText] -> Int -> Int -> IO ComboBox
addComboBox grid options left top = do
    cbox <- comboBoxNewText
    forM_ options $ comboBoxAppendText cbox
    comboBoxSetActive cbox 0
    gridAttach grid cbox left top 1 1
    return cbox

askDeleteField :: [String] -> GUIControl -> IO ()
askDeleteField names control = do
    dlg <- dialogNew
    set dlg [ windowTransientFor := mainWindow control
            , windowModal := True
            ]
    dialogAddButton dlg
                    ("Borrar" :: String)
                    ResponseOk
    dialogAddButton dlg
                    ("Cancelar" :: String)
                    ResponseCancel
    content <- castToContainer <$> dialogGetContentArea dlg
    labelNew (Just ("Borrar Campos" :: String)) >>= containerAdd content

    grid <- gridNew
    cbuttons <- forM (enumerate names) $ \(row, name) -> addCheckButton grid name 0 row

    actionArea <- castToContainer <$> dialogGetActionArea dlg
    containerAdd actionArea grid

    widgetShowAll dlg
    r <- dialogRun dlg

    when (r == ResponseOk) $ do
        fields <- map fst <$> filterM (toggleButtonGetActive . snd) (enumerate cbuttons)
        unless (null fields) $ sendInput control $ DeleteFields fields
    widgetDestroy dlg


getFieldFormula :: FieldPos -> String -> Maybe String -> GUIControl -> IO ()
getFieldFormula fieldPos fieldName mFormula control = do
    let dlg = changeFieldFormulaDialog control
        btn = changeFieldFormulaButton control
        entry = changeFieldFormulaEntry control
        lbl = changeFieldFormulaLabel control
    set dlg [ windowTransientFor := mainWindow control
            , windowModal := True
            ]

    toggleButtonSetActive btn $ isJust mFormula
    entrySetText entry $ fromMaybe "" mFormula
    widgetSetSensitive entry $ isJust mFormula
    entry `on` keyPressEvent $ do
      name <- eventKeyName
      mods <- eventModifier
      if null mods && name == "Return"
      then liftIO (dialogResponse dlg ResponseOk) >> return True
      else return False
    labelSetText lbl $ fieldName ++ " = "

    widgetShowAll dlg
    r <- dialogRun dlg
    widgetHide dlg
    -- putStrLn $ "Response: " ++ show r
    when (r == ResponseOk) $ do
        active <- toggleButtonGetActive btn
        f <- entryGetText entry
        sendInput control $ ChangeFieldFormula (if active
                                                then Just f
                                                else Nothing) fieldPos

searchField :: FieldPos -> String -> [String] -> GUIControl -> IO ()
searchField fieldPos initial values control = do
    let dlg = searchFieldDialog control
        combo = searchFieldCombo control

    set dlg [ windowTransientFor := mainWindow control
            , windowModal := True
            ]

    modelText <- comboBoxGetModelText combo
    listStoreClear modelText
    forM values $ comboBoxAppendText combo . T.pack
    comboBoxSetActive combo . fromJust $ elemIndex initial values

    widgetShowAll dlg
    r <- dialogRun dlg
    widgetHide dlg
    when (r == ResponseOk) $ do
        mt <- comboBoxGetActiveText combo
        when (isJust mt) $ sendInput control $ MoveToValue fieldPos (T.unpack $ fromJust mt)

unimplemented :: String -> GUIControl -> IO ()
unimplemented func control = sendInput control . MessageDialog . ErrorMessage $ "Función " ++ func ++ " no implementada"
