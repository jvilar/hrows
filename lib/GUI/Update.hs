{-# LANGUAGE OverloadedStrings
           , OverloadedLabels
#-}

module GUI.Update (
            -- *Types
            GUICommand
            -- *Functions
            , updateGUI
) where

import Control.Monad(filterM, forM, forM_, guard, unless, when)
import Control.Monad.IO.Class(liftIO)
import Data.Bits(Bits(setBit, clearBit))
import Data.BitVector(nil, BV, extract, (#), ones, (@.))
import qualified Data.BitVector as BV
import Data.Either(lefts, rights)
import Data.IORef(IORef, modifyIORef, readIORef, writeIORef)
import Data.List(elemIndex, sort)
import Data.Maybe(catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Text(Text)
import qualified Data.Text as T
import GHC.Int(Int32)
import GI.Gtk hiding (MessageDialog)
import GI.Gdk
import TextShow(TextShow(showt))

import GUI.Command
import GUI.Control
import Model hiding (deleteFields)
import Model.DefaultFileNames
import Presenter.ImportType
import Presenter.Input

updateGUI :: GUICommand -> GUIControl -> IO ()
updateGUI (ChangeTitle title) = changeTitle title
updateGUI (ShowPosition pos size) = updatePosition pos size
updateGUI (ShowFields fis) = showFields fis
updateGUI (ShowNames names) = updateNames names
updateGUI (ShowIteration iter) = showIteration iter
updateGUI DisableTextViews = disableTextViews


changeTitle :: Text -> GUIControl -> IO ()
changeTitle title control = set (mainWindow control) [ windowTitle := title ]

updatePosition :: Int -> Int -> GUIControl -> IO ()
updatePosition pos size control = do
    beginButton control `set` [ #sensitive := pos > 1 ]
    leftButton control `set` [ #sensitive := pos > 1 ]
    endButton control `set` [ #sensitive := pos < size ]
    rightButton control `set` [ #sensitive := pos < size ]

    labelSetText (positionLabel control) $ T.concat [showt pos, "/", showt size]

enumerate :: Integral int => [a] -> [(int, a)]
enumerate = zip [0..]

showFields :: [FieldInfo] -> GUIControl -> IO ()
showFields fis control = do
  let grid = fieldsGrid control

  forM_ fis $ \fi -> do
                       textView <- recoverTextView (indexFI fi) control
                       disconnectTextView (indexFI fi) control
                       let tooltip = fromMaybe (typeLabel $ typeFI fi) $ formulaFI fi
                       label <- recoverLabel (indexFI fi) control
                       label `set` [ #tooltipText := tooltip ]
                       set textView [ textViewEditable := isNothing $ formulaFI fi
                                    , widgetCanFocus := isNothing $ formulaFI fi
                                    , #name := if isErrorFI fi
                                               then "error"
                                               else if isJust $ formulaFI fi
                                                    then "formula"
                                                    else "normal"
                                    ]
                       #setStateFlags textView [StateFlagsNormal] True

                       buffer <- textViewGetBuffer textView
                       forM_ (textFI fi) $ \t ->
                            textBufferSetText buffer t (fromIntegral $ T.length t)
                       reconnectTextView (indexFI fi) control
  widgetShowAll grid

recoverTextView :: FieldPos -> GUIControl -> IO TextView
recoverTextView row control = do
    -- putStrLn $ "Recovering textView in row " ++ show row
    Just tv <- gridGetChildAt (fieldsGrid control) 1 row
    unsafeCastTo TextView tv

recoverLabel :: FieldPos -> GUIControl -> IO Label
recoverLabel row control = do
    -- putStrLn $ "Recovering label in row " ++ show row
    Just ebox <- gridGetChildAt (fieldsGrid control) 0 row
    cbox <- unsafeCastTo EventBox ebox
    lbl <- head <$> containerGetChildren cbox
    unsafeCastTo Label lbl

addTextBufferActive :: Int -> GUIControl -> IO ()
addTextBufferActive n control = modifyIORef (textBufferActive control) (\bv -> ones n # bv)

deleteTextBufferActive :: [FieldPos] -> GUIControl -> IO ()
deleteTextBufferActive ps control = let
    survivors :: Int32 -> [Int32] -> BV -> BV
    survivors l [] bv = add l (fromIntegral $ BV.size bv - 1) bv nil
    survivors l (p:ps) bv = add l (p-1) bv $ survivors (p+1) ps bv
    add :: Int32 -> Int32 -> BV -> BV -> BV
    add l r bv | l <= r = (extract r l bv #)
               | otherwise = id
  in modifyIORef (textBufferActive control) (survivors 0 ps)

disableTextViews :: GUIControl -> IO ()
disableTextViews control = do
  nfields <- readIORef $ numberOfFields control
  forM_ [0 .. fromIntegral nfields-1] $ \f -> do
                                textView <- recoverTextView f control
                                set textView [ #editable := False
                                             , #canFocus := False
                                             , #sensitive := False
                                             , #name := "empty"
                                             ]


updateNames :: [Name] -> GUIControl -> IO ()
updateNames names control = do
  let grid = fieldsGrid control
  adjustTextFields (length names) control

  forM_ (enumerate names) $ \(r, name) -> do
                             label <- recoverLabel r control
                             labelSetText label name
  widgetShowAll grid

adjustTextFields :: Int -> GUIControl -> IO ()
adjustTextFields nfields control = do
  let grid = fieldsGrid control
  current <- readIORef $ numberOfFields control
  case compare current nfields of
    LT -> addFields grid [fromIntegral current .. fromIntegral nfields - 1] control
    EQ -> return ()
    GT -> deleteFields grid [fromIntegral nfields .. fromIntegral current - 1] control
  writeIORef (numberOfFields control) nfields

addFields :: Grid -> [FieldPos] -> GUIControl -> IO ()
addFields grid fields control = do
    forM fields $ \f -> do
       lbl <- createFieldLabel f control
       #attach grid lbl 0 f 1 1
       textView <- createFieldTextView f control
       #attach grid textView 1 f 1 1
    addTextBufferActive (length fields) control

createFieldLabel :: FieldPos -> GUIControl -> IO EventBox
createFieldLabel f control = do
         lbl <- labelNew $ Just ""
         #setHalign lbl AlignStart
         ebox <- eventBoxNew
         #dragSourceSet ebox [ModifierTypeButton1Mask] Nothing [DragActionMove] -- Check the Nothing, I have no idea if it is correct
         #dragSourceSetTargetList ebox (Just $ targetList control)
         #dragDestSet ebox [DestDefaultsAll] Nothing [DragActionMove] -- Ditto for Nothing
         #dragDestSetTargetList ebox (Just $ targetList control)
         ebox `on` #dragDataGet $ \_ sdata _ _ -> do
                                   let (t,l) = (showt f, fromIntegral $ T.length t)
                                   ok <- selectionDataSetText sdata t l
                                   unless ok (liftIO $ dndError control)
         ebox `on` #dragDataReceived $ \_ _ _ sdata _ _ -> do
                               t <- selectionDataGetText sdata
                               liftIO $ case (t :: Maybe Text) of
                                          Nothing -> dndError control
                                          Just v -> let
                                                      from = read $ T.unpack v
                                                    in if from /= f
                                                       then sendInput control $ MoveField from f
                                                       else return ()
         #add ebox lbl
         return ebox

dndError :: GUIControl -> IO ()
dndError control = sendInput control $ MessageDialog (ErrorMessage "Algo está mal en el dnd")

disconnectTextView :: FieldPos -> GUIControl -> IO ()
disconnectTextView t control = modifyIORef (textBufferActive control) (flip clearBit $ fromIntegral t)

reconnectTextView :: FieldPos -> GUIControl -> IO ()
reconnectTextView t control = modifyIORef (textBufferActive control) (flip setBit $ fromIntegral t)


createFieldTextView :: FieldPos -> GUIControl -> IO TextView
createFieldTextView f control = do
         textView <- textViewNew
         set textView [ #wrapMode := WrapModeWord
                      , #acceptsTab := False
                      , #editable := False
                      , #sensitive := False
                      , #canFocus := False
                      , #hexpand := True
                      ]
         buffer <- textViewGetBuffer textView

         buffer `on` #changed $ liftIO $ do
             isActive <- (@. f) <$> readIORef (textBufferActive control)
             when isActive $ do
                 begin <- #getStartIter buffer
                 end <- #getEndIter buffer
                 text <- #getText buffer begin end False
                 sendInput control $ UpdateField f (toField text)

         textView `on` #buttonPressEvent $ \event -> do
             button <- get event #button
             if button == 3
             then liftIO $ do
                      writeIORef (currentField control) f
                      #popupAtPointer (fieldMenu control) Nothing
                      return True
             else return False
         return textView

deleteFields :: Grid -> [FieldPos] -> GUIControl -> IO ()
deleteFields grid fields control = do
                      forM_ fields $ \f ->
                         forM_ [0, 1] $ \c -> do
                             Just w <- gridGetChildAt grid c f
                             widgetDestroy w
                      deleteTextBufferActive fields control

showIteration :: Iteration -> GUIControl -> IO ()
showIteration AskReadFile = askReadFile
showIteration AskWriteFile = askWriteFile
showIteration AskCreateField = askCreateField
showIteration (AskDeleteFields fs) = askDeleteFields fs
showIteration (AskImportFrom t) = askImportFrom t
showIteration (AskImportOptions t ifs cfs m) = askImportOptions t ifs cfs m
showIteration (AskRenameFields fs) = askRenameFields fs
showIteration (AskSortRows fs) = askSortRows fs
showIteration (DisplayMessage m) = displayMessage m
showIteration (ConfirmExit changed) = confirmExit changed
showIteration (GetFieldFormula fpos flabel ms) = getFieldFormula fpos flabel ms
showIteration (SearchField fpos initial l) = searchField fpos initial l
showIteration (CopyOtherField fpos initial l) = copyOther fpos initial l
showIteration it = unimplemented $ show it

displayMessage :: Message -> GUIControl -> IO ()
displayMessage (ErrorMessage m) = noResponseMessage m
displayMessage (WarningMessage m) = noResponseMessage m
displayMessage (InformationMessage m) = noResponseMessage m
displayMessage (QuestionMessage _) = undefined

noResponseMessage :: Text -> GUIControl -> IO ()
noResponseMessage m control = do
    dlg <- dialogNew
    set dlg [ #transientFor := mainWindow control
            , #modal := True
            , #typeHint := WindowTypeHintDialog
            , #windowPosition := WindowPositionCenterOnParent
            ]
    #addButton dlg "Ok" $ asInt32 ResponseTypeOk
    content <- #getContentArea dlg
    message <- labelNew $ Just m
    #packStart content message True True 2
    #showAll dlg
    #run dlg
    widgetDestroy dlg

askReadFile :: GUIControl -> IO ()
askReadFile = askFile loadFileDialog confFileLoadCheckButton LoadFileFromName

askWriteFile :: GUIControl -> IO ()
askWriteFile = askFile saveAsDialog confFileSaveCheckButton WriteFileFromName

isResponse :: Int32 -> ResponseType -> Bool
isResponse r = (== r) . fromIntegral . fromEnum

asInt32 :: Enum e => e -> Int32
asInt32 = fromIntegral . fromEnum

askFile :: IsInput t => (GUIControl -> FileChooserDialog)
                     -> (GUIControl -> CheckButton)
                     -> (FilePath -> Maybe FilePath -> t)
                     -> GUIControl -> IO ()
askFile dlg btn input control = do
    let dialog = dlg control
    set dialog [ #transientFor := mainWindow control
               , #modal := True
               , #typeHint := WindowTypeHintDialog
               , #windowPosition := WindowPositionCenterOnParent
               ]
    r <- #run dialog
    #hide dialog
    when (isResponse r ResponseTypeOk) $ do
            file <- fileChooserGetFilename dialog
            when (isJust file) $ do
                chk <- toggleButtonGetActive (btn control)
                let fp = fromJust file
                    conf = if chk
                           then defaultConfFileName <$> file
                           else Nothing
                sendInput control $ input fp conf

askImportFrom :: ImportType -> GUIControl -> IO ()
askImportFrom t control = do
    let dialog = importFromFileDialog control
    set dialog [ #transientFor := mainWindow control
               , #modal := True
               , #typeHint := WindowTypeHintDialog
               , #windowPosition := WindowPositionCenterOnParent
               ]
    r <- #run dialog
    #hide dialog
    when (isResponse r ResponseTypeOk) $ do
        file <- fileChooserGetFilename dialog
        separator <- translateChar <$> entryGetText (importInputSeparator control)
        when (isJust file) $ sendInput control $ ImportFromFileName t (fromJust file) separator

askImportOptions :: ImportType -> [Name] -> [Name] -> Model -> GUIControl -> IO ()
askImportOptions t ifs cfs m control = do
    let (dialog, grid) = case t of
          ImportFields -> (importFieldsOptionsDialog control, importFieldsOptionsRows control)
          ImportRows -> (importRowsOptionsDialog control, importRowsOptionsRows control)

    gridSetRowSpacing grid 3
    gridSetColumnSpacing grid 9
    children <- containerGetChildren grid
    forM_ children widgetDestroy

    let ifst = "" : ifs
        options = case t of
                    ImportFields -> ["", "<-", "=="] :: [Text]
                    ImportRows -> ["", "<-"]
    forM_ (enumerate cfs) $ \(row, current) -> do
        lbl <- addLabel grid current 0 row
        #setHalign lbl AlignStart
        btn <- addButton grid "" 1 row
        btn `on` #activate $ do
            l <- buttonGetLabel btn
            let Just n = elemIndex l options
                n' = (n+1) `mod` (length options)
            buttonSetLabel btn (options !! n')
        addComboBox grid ifst 2 row

    widgetShowAll dialog
    set dialog [ #transientFor := mainWindow control
               , #modal := True
               , #typeHint := WindowTypeHintDialog
               , #windowPosition := WindowPositionCenterOnParent
               ]
    r <- #run dialog
    #hide dialog

    when (isResponse r ResponseTypeOk) $ do
        l <- catMaybes <$> (forM (enumerate cfs) $ \(row, _) -> do
            Just b <- gridGetChildAt grid 1 row
            Just btn <- castTo Button b
            option <- buttonGetLabel btn
            Just cb <- gridGetChildAt grid 2 row
            Just cbox <- castTo ComboBox cb
            i <- comboBoxGetActive cbox
            return $ if i == 0 || T.null option
                     then Nothing
                     else Just $ case option of
                                     "<-" -> Left (row, i - 1)
                                     "==" -> Right (row, i - 1)
           )
        let keys = rights l
            values = lefts l
        sendInput control $ case t of
                              ImportFields -> ImportFieldsFromModel m keys values
                              ImportRows -> ImportRowsFromModel m values

translateChar :: Text -> Char
translateChar t = case T.uncons t of
                    Nothing -> '\t'
                    Just ('\\', t') -> if T.null t'
                                       then '\\'
                                       else T.head t'
                    Just (c, _) -> c

confirmExit :: Bool -> GUIControl -> IO ()
confirmExit changed control = do
  let msg :: String
      msg = if changed
            then "Ha habido cambios, ¿cómo quieres salir?"
            else "¿Seguro que quieres salir?"
  dlg <- dialogNew
  set dlg [ #transientFor := mainWindow control
          , #modal := True
          , #typeHint := WindowTypeHintDialog
          , #windowPosition := WindowPositionCenterOnParent
          ]
  if changed
  then do
         #addButton dlg "Grabar y salir" 1
         #addButton dlg "Salir sin grabar" $ asInt32 ResponseTypeYes
         #addButton dlg "No salir" $ asInt32 ResponseTypeNo
  else do
         #addButton dlg "Sí" $ asInt32 ResponseTypeYes
         #addButton dlg "No" $ asInt32 ResponseTypeNo

  r <- #run dlg
  when (isResponse r ResponseTypeYes) $ do
                        sendInput control ExitProgram
                        mainQuit
  when (r == 1) $ do
                   sendInput control WriteFile
                   sendInput control ExitProgram
                   mainQuit
  widgetDestroy dlg

askCreateField :: GUIControl -> IO ()
askCreateField control = do
    dlg <- dialogNew
    set dlg [ #transientFor := mainWindow control
            , #modal := True
            , #typeHint := WindowTypeHintDialog
            , #windowPosition := WindowPositionCenterOnParent
            ]
    #addButton dlg "Crear" $ asInt32 ResponseTypeOk
    #addButton dlg "Cancelar" $ asInt32 ResponseTypeCancel
    Just content <- #getContentArea dlg >>= castTo Container
    labelNew (Just "Crear Campos") >>= #add content

    grid <- gridNew
    addLabel grid "Nombre" 0 0
    addLabel grid "Tipo" 1 0
    entries <- forM [1..10] $ \row -> (,)
                                    <$> addEntry grid 0 row
                                    <*> addComboBox grid (map snd typeLabels) 1 row

    Just actionArea <- #getActionArea dlg >>= castTo Container
    #add actionArea grid

    widgetShowAll dlg
    r <- #run dlg

    when (isResponse r ResponseTypeOk) $ do
        fields <- catMaybes <$> forM entries (\(entry, cbox) -> do
                                        name <- entryGetText entry
                                        i <- comboBoxGetActive cbox
                                        return $ if T.null name || i == -1
                                                 then Nothing
                                                 else Just (name, fst $ typeLabels !!! i)
                                )
        unless (null fields) $ sendInput control $ NewFields fields
    widgetDestroy dlg

addLabel :: Grid -> Text -> Int32 -> Int32 -> IO Label
addLabel grid text left top = do
    lbl <- labelNew $ Just text
    #attach grid lbl left top 1 1
    return lbl

addButton :: Grid -> Text -> Int32 -> Int32 -> IO Button
addButton grid label left top = do
    btn <- buttonNewWithLabel label
    #attach grid btn left top 1 1
    return btn

addRadioButton :: Grid -> Text -> Int32 -> Int32 -> IO RadioButton
addRadioButton grid label left top = do
    btn <- radioButtonNewWithLabel ([] :: [RadioButton]) label
    #attach grid btn left top 1 1
    return btn

addRadioButtonFromWidget :: Grid -> RadioButton -> Text -> Int32 -> Int32 -> IO RadioButton
addRadioButtonFromWidget grid other label left top = do
    btn <- radioButtonNewWithLabelFromWidget (Just other) label
    #attach grid btn left top 1 1
    return btn

addCheckButton :: Grid -> Text -> Int32 -> Int32 -> IO CheckButton
addCheckButton grid label left top = do
    btn <- checkButtonNewWithLabel label
    #attach grid btn left top 1 1
    return btn

addSimpleCheckButton :: Grid -> Int32 -> Int32 -> IO CheckButton
addSimpleCheckButton grid left top = do
    btn <- checkButtonNew
    #attach grid btn left top 1 1
    return btn

addEntry :: Grid -> Int32 -> Int32 -> IO Entry
addEntry grid left top = do
    entry <- entryNew
    #attach grid entry left top 1 1
    return entry

addComboBox :: Grid -> [Text] -> Int32 -> Int32 -> IO ComboBoxText
addComboBox grid options left top = do
    cbox <- comboBoxTextNew
    forM_ options $ #append cbox Nothing
    #setActive cbox 0
    #attach grid cbox left top 1 1
    return cbox

askDeleteFields :: [Name] -> GUIControl -> IO ()
askDeleteFields names control = do
    dlg <- dialogNew
    set dlg [ #transientFor := mainWindow control
            , #modal := True
            , #typeHint := WindowTypeHintDialog
            , #windowPosition := WindowPositionCenterOnParent
            ]
    #addButton dlg "Borrar" $ asInt32 ResponseTypeOk
    #addButton dlg "Cancelar" $ asInt32 ResponseTypeCancel
    Just content <- #getContentArea dlg >>= castTo Container
    labelNew (Just "Borrar Campos") >>= #add content

    grid <- gridNew
    cbuttons <- forM (enumerate names) $ \(row, name) -> addCheckButton grid name 0 row

    Just actionArea <- #getActionArea dlg >>= castTo Container
    #add actionArea grid

    widgetShowAll dlg
    r <- #run dlg

    when (isResponse r ResponseTypeOk) $ do
        fields <- map fst <$> filterM (toggleButtonGetActive . snd) (enumerate cbuttons)
        unless (null fields) $ sendInput control $ DeleteFields fields
    widgetDestroy dlg

askRenameFields :: [Name] -> GUIControl -> IO ()
askRenameFields names control = do
    dlg <- dialogNew
    set dlg [ windowTransientFor := mainWindow control
            , windowModal := True
            ]
    #addButton dlg "Cambiar" $ asInt32 ResponseTypeOk
    #addButton dlg "Cancelar" $ asInt32  ResponseTypeCancel
    Just content <- #getContentArea dlg >>= castTo Container
    labelNew (Just "Cambiar Nombres Campos") >>= #add content

    grid <- gridNew
    centries <- forM (enumerate names) $ \(row, name) -> do
        addLabel grid name 0 row
        entry <- addEntry grid 1 row
        entrySetText entry name
        return entry

    Just actionArea <- #getActionArea dlg >>= castTo Container
    #add actionArea grid

    widgetShowAll dlg
    r <- #run dlg

    when (isResponse r ResponseTypeOk) $ do
        names <- mapM entryGetText centries
        sendInput control $ RenameFields names
    widgetDestroy dlg

askSortRows :: [Name] -> GUIControl -> IO ()
askSortRows names control = do
    dlg <- dialogNew
    set dlg [ windowTransientFor := mainWindow control
            , windowModal := True
            ]
    #addButton dlg "Ascendente" 1
    #addButton dlg "Descendente" 2
    #addButton dlg "Cancelar" 3
    Just content <- #getContentArea dlg >>= castTo Container
    labelNew (Just "Ordenar") >>= #add content

    grid <- gridNew

    btn <- addRadioButton grid (head names) 0 0
    toggleButtonSetActive btn True
    cbuttons <- (btn :) <$> (forM (enumerate $ tail names) $ \(row, name) ->
        addRadioButtonFromWidget grid btn name 0 (row + 1))

    Just actionArea <- #getActionArea dlg >>= castTo Container
    #add actionArea grid

    widgetShowAll dlg
    r <- #run dlg

    unless (r == 3) $ do
        fp <- fst . head . filter snd . enumerate <$> mapM toggleButtonGetActive cbuttons
        let order = case r of
                        1 -> Ascending
                        2 -> Descending
        sendInput control $ SortRows fp order
    widgetDestroy dlg


getFieldFormula :: FieldPos -> Name -> Maybe Formula -> GUIControl -> IO ()
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
    #setActivatesDefault entry True
{- TODO    entry `on` keyPressEvent $ do
      name <- eventKeyName
      mods <- eventModifier
      if null mods && name == "Return"
      then liftIO (dialogResponse dlg ResponseTypeOk) >> return True
      else return False -}
    labelSetText lbl $ fieldName `T.append` " = "

    widgetShowAll dlg
    r <- #run dlg
    #hide dlg
    -- putStrLn $ "Response: " ++ show r
    when (isResponse r ResponseTypeOk) $ do
        active <- toggleButtonGetActive btn
        f <- entryGetText entry
        sendInput control $ ChangeFieldFormula (if active
                                                then Just f
                                                else Nothing) fieldPos

useCombo :: Dialog -> ComboBoxText -> Text -> [Text] -> GUIControl -> IO (Maybe Text)
useCombo dlg combo initial values control = do
    set dlg [ windowTransientFor := mainWindow control
            , windowModal := True
            ]

    #removeAll combo
    forM values $ #appendText combo
    comboBoxSetActive combo . fromIntegral . fromJust $ elemIndex initial values

    widgetShowAll dlg
    r <- #run dlg
    #hide dlg
    if (isResponse r ResponseTypeOk)
    then Just <$> #getActiveText combo
    else return Nothing

searchField :: FieldPos -> Text -> [Text] -> GUIControl -> IO ()
searchField fieldPos initial values control = do
    let dlg = searchFieldDialog control
        combo = searchFieldCombo control

    mt <- useCombo dlg combo initial values control
    when (isJust mt) $ sendInput control $ MoveToValue fieldPos (fromJust mt)

copyOther :: FieldPos -> Text -> [Text] -> GUIControl -> IO ()
copyOther fieldPos initial values control = do
    let dlg = copyOtherDialog control
        combo = copyOtherCombo control

    mt <- useCombo dlg combo initial values control
    when (isJust mt) $ do
      textView <- recoverTextView fieldPos control
      editable <- get textView textViewEditable
      when editable $ do
         buffer <- textViewGetBuffer textView
         let (t,l) = (fromJust mt, fromIntegral $ T.length t)
         textBufferSetText buffer t l

unimplemented :: String -> GUIControl -> IO ()
unimplemented func control = sendInput control . MessageDialog . ErrorMessage $ T.concat ["Función ", T.pack func, " no implementada"]
