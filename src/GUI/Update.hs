{-# LANGUAGE OverloadedStrings #-}

module GUI.Update (
            -- *Types
            GUICommand
            -- *Functions
            , updateGUI
) where

import Control.Concurrent.Chan(writeChan)
import Control.Monad(filterM, forM, forM_, guard, unless, when)
import Control.Monad.IO.Class(liftIO)
import Data.Either(lefts, rights)
import Data.IORef(readIORef, writeIORef)
import Data.List(elemIndex)
import Data.Maybe(catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Text(Text)
import qualified Data.Text as T
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums(Align(..))

import GUI.Command
import GUI.Control
import Model hiding (deleteFields)
import Model.DefaultFileNames
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
         buffer `on` bufferChanged $ liftIO $ do
             begin <- textBufferGetStartIter buffer
             end <- textBufferGetEndIter buffer
             text <- textBufferGetText buffer begin end False
             writeChan (inputChan control) (toInput $ UpdateField f (toField (text::String)))

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
showIteration (AskDeleteFields fs) = askDeleteFields fs
showIteration AskImportFieldsFrom = askImportFieldsFrom
showIteration (AskImportFieldsOptions ifs cfs m) = askImportFieldsOptions ifs cfs m
showIteration (AskRenameFields fs) = askRenameFields fs
showIteration (AskSortRows fs) = askSortRows fs
showIteration (DisplayMessage m) = displayMessage m
showIteration (ConfirmExit changed) = confirmExit changed
showIteration (GetFieldFormula fpos flabel ms) = getFieldFormula fpos flabel ms
showIteration (SearchField fpos initial l) = searchField fpos initial l
showIteration it = unimplemented $ show it

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
                     -> (FilePath -> Maybe FilePath -> t)
                     -> GUIControl -> IO ()
askFile dlg btn input control = do
    let dialog = dlg control
    set dialog [ windowTransientFor := mainWindow control
               , windowModal := True
               ]
    r <- dialogRun dialog
    widgetHide dialog
    when (r == ResponseOk) $ do
            file <- fileChooserGetFilename dialog
            when (isJust file) $ do
                chk <- toggleButtonGetActive (btn control)
                let fp = fromJust file
                    conf = if chk
                           then defaultConfFileName <$> file
                           else Nothing
                sendInput control $ input fp conf

askImportFieldsFrom :: GUIControl -> IO ()
askImportFieldsFrom control = do
    let dialog = importFieldsFromFileDialog control
    set dialog [ windowTransientFor := mainWindow control
               , windowModal := True
               ]
    r <- dialogRun dialog
    widgetHide dialog
    when (r == ResponseOk) $ do
        file <- fileChooserGetFilename dialog
        separator <- translateChar <$> entryGetText (importFieldsInputSeparator control)
        when (isJust file) $ sendInput control $ ImportFieldsFromFileName (fromJust file) separator

askImportFieldsOptions :: [String] -> [String] -> Model -> GUIControl -> IO ()
askImportFieldsOptions ifs cfs m control = do
    let dialog = importFieldsOptionsDialog control
    let grid = importFieldsOptionsRows control

    gridSetRowSpacing grid 3
    gridSetColumnSpacing grid 9
    children <- containerGetChildren grid
    forM_ children widgetDestroy

    let ifst = "" : map T.pack ifs
        options = ["", "<-", "=="] :: [Text]
    forM_ (enumerate cfs) $ \(row, current) -> do
        lbl <- addLabel grid current 0 row
        widgetSetHAlign lbl AlignStart
        btn <- addButton grid "" 1 row
        btn `on` buttonActivated $ do
            l <- buttonGetLabel btn
            let Just n = elemIndex l options
                n' = (n+1) `mod` (length options)
            buttonSetLabel btn (options !! n')
        addComboBox grid ifst 2 row

    widgetShowAll dialog
    r <- dialogRun dialog
    set dialog [ windowTransientFor := mainWindow control
               , windowModal := True
               , windowTypeHint := WindowTypeHintDialog
               , windowWindowPosition := WinPosCenterOnParent
               ]
    widgetHide dialog

    when (r == ResponseOk) $ do
        l <- catMaybes <$> (forM (enumerate cfs) $ \(row, _) -> do
            Just b <- gridGetChildAt grid 1 row
            let btn = castToButton b
            option <- buttonGetLabel btn
            Just cb <- gridGetChildAt grid 2 row
            let cbox = castToComboBox cb
            i <- comboBoxGetActive cbox
            return $ if i == 0 || option == ("" :: Text)
                     then Nothing
                     else Just $ case option of
                                     "<-" -> Left (row, i - 1)
                                     "==" -> Right (row, i - 1)
           )
        let keys = rights l
            values = lefts l
        sendInput control $ ImportFieldsFromModel m keys values

translateChar :: String -> Char
translateChar "\\t" = '\t'
translateChar ('\\':[c]) = c
translateChar (c:_) = c

confirmExit :: Bool -> GUIControl -> IO ()
confirmExit changed control = do
  let msg :: String
      msg = if changed
            then "Ha habido cambios, ¿cómo quieres salir?"
            else "¿Seguro que quieres salir?"
  dlg <- messageDialogNew (Just $ mainWindow control)
                          [DialogModal]
                          MessageQuestion
                          (if changed
                           then ButtonsNone
                           else ButtonsYesNo)
                          msg
  when changed $ do
      dialogAddButton dlg ("Grabar y salir" :: String) (ResponseUser 1)
      dialogAddButton dlg ("Salir sin grabar" :: String) ResponseYes
      dialogAddButton dlg ("No salir" :: String) ResponseNo
      return ()

  r <- dialogRun dlg
  when (r == ResponseYes) $ do
                        sendInput control ExitProgram
                        mainQuit
  when (r == ResponseUser 1) $ do
                        sendInput control WriteFile
                        sendInput control ExitProgram
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
    entries <- forM [1..10] $ \row -> (,)
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

addLabel :: Grid -> String -> Int -> Int -> IO Label
addLabel grid text left top = do
    lbl <- labelNew $ Just text
    gridAttach grid lbl left top 1 1
    return lbl

addButton :: Grid -> String -> Int -> Int -> IO Button
addButton grid label left top = do
    btn <- buttonNewWithLabel label
    gridAttach grid btn left top 1 1
    return btn

addRadioButton :: Grid -> String -> Int -> Int -> IO RadioButton
addRadioButton grid label left top = do
    btn <- radioButtonNewWithLabel label
    gridAttach grid btn left top 1 1
    return btn

addRadioButtonFromWidget :: Grid -> RadioButton -> String -> Int -> Int -> IO RadioButton
addRadioButtonFromWidget grid other label left top = do
    btn <- radioButtonNewWithLabelFromWidget other label
    gridAttach grid btn left top 1 1
    return btn

addCheckButton :: Grid -> String -> Int -> Int -> IO CheckButton
addCheckButton grid label left top = do
    btn <- checkButtonNewWithLabel label
    gridAttach grid btn left top 1 1
    return btn

addSimpleCheckButton :: Grid -> Int -> Int -> IO CheckButton
addSimpleCheckButton grid left top = do
    btn <- checkButtonNew
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

askDeleteFields :: [String] -> GUIControl -> IO ()
askDeleteFields names control = do
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

askRenameFields :: [String] -> GUIControl -> IO ()
askRenameFields names control = do
    dlg <- dialogNew
    set dlg [ windowTransientFor := mainWindow control
            , windowModal := True
            ]
    dialogAddButton dlg
                    ("Cambiar" :: String)
                    ResponseOk
    dialogAddButton dlg
                    ("Cancelar" :: String)
                    ResponseCancel
    content <- castToContainer <$> dialogGetContentArea dlg
    labelNew (Just ("Cambiar Nombres Campos" :: String)) >>= containerAdd content

    grid <- gridNew
    centries <- forM (enumerate names) $ \(row, name) -> do
        addLabel grid name 0 row
        entry <- addEntry grid 1 row
        entrySetText entry name
        return entry

    actionArea <- castToContainer <$> dialogGetActionArea dlg
    containerAdd actionArea grid

    widgetShowAll dlg
    r <- dialogRun dlg

    when (r == ResponseOk) $ do
        names <- mapM entryGetText centries
        sendInput control $ RenameFields names
    widgetDestroy dlg

askSortRows :: [String] -> GUIControl -> IO ()
askSortRows names control = do
    dlg <- dialogNew
    set dlg [ windowTransientFor := mainWindow control
            , windowModal := True
            ]
    dialogAddButton dlg
                    ("Ascendente" :: String)
                    (ResponseUser 1)
    dialogAddButton dlg
                    ("Descendente" :: String)
                    (ResponseUser 2)
    dialogAddButton dlg
                    ("Cancelar" :: String)
                    ResponseCancel
    content <- castToContainer <$> dialogGetContentArea dlg
    labelNew (Just ("Ordenar" :: String)) >>= containerAdd content

    grid <- gridNew

    btn <- addRadioButton grid (head names) 0 0
    toggleButtonSetActive btn True
    cbuttons <- (btn :) <$> (forM (enumerate $ tail names) $ \(row, name) ->
        addRadioButtonFromWidget grid btn name 0 (row + 1))

    actionArea <- castToContainer <$> dialogGetActionArea dlg
    containerAdd actionArea grid

    widgetShowAll dlg
    r <- dialogRun dlg

    unless (r == ResponseCancel) $ do
        fp <- fst . head . filter snd . enumerate <$> mapM toggleButtonGetActive cbuttons
        let order = case r of
                        ResponseUser 1 -> Ascending
                        ResponseUser 2 -> Descending
        sendInput control $ SortRows fp order
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
