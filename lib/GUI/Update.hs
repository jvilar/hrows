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
import Data.Bits(Bits(..))
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
import GUI.MainWindow
import GUI.MainWindow.Update
import Model hiding (deleteFields)
import Model.DefaultFileNames
import Presenter.ImportType
import Presenter.Input

updateGUI :: GUICommand -> GUIControl -> IO ()
updateGUI (ChangeTitle title) = changeTitle title . mainWindow
updateGUI (ShowPosition pos size) = updatePosition pos size . mainWindow
updateGUI (ShowFields fis) = showFields fis . mainWindow
updateGUI (ShowNames names) = updateNames names . mainWindow
updateGUI (ShowIteration iter) = showIteration iter
updateGUI DisableTextViews = disableTextViews . mainWindow

enumerate :: Integral int => [a] -> [(int, a)]
enumerate = zip [0..]

dndError :: GUIControl -> IO ()
dndError control = sendInput control $ MessageDialog (ErrorMessage "Algo está mal en el dnd")

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
    set dlg [ #transientFor := window $ mainWindow control
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
    #destroy dlg

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
    set dialog [ #transientFor := window $ mainWindow control
               , #modal := True
               , #typeHint := WindowTypeHintDialog
               , #windowPosition := WindowPositionCenterOnParent
               ]
    r <- #run dialog
    #hide dialog
    when (isResponse r ResponseTypeOk) $ do
            file <- #getFilename dialog
            when (isJust file) $ do
                chk <- #getActive (btn control)
                let fp = fromJust file
                    conf = if chk
                           then defaultConfFileName <$> file
                           else Nothing
                sendInput control $ input fp conf

askImportFrom :: ImportType -> GUIControl -> IO ()
askImportFrom t control = do
    let dialog = importFromFileDialog control
    set dialog [ #transientFor := window $ mainWindow control
               , #modal := True
               , #typeHint := WindowTypeHintDialog
               , #windowPosition := WindowPositionCenterOnParent
               ]
    r <- #run dialog
    #hide dialog
    when (isResponse r ResponseTypeOk) $ do
        file <- #getFilename dialog
        separator <- translateChar <$> #getText (importInputSeparator control)
        when (isJust file) $ sendInput control $ ImportFromFileName t (fromJust file) separator

askImportOptions :: ImportType -> [FieldName] -> [FieldName] -> Model -> GUIControl -> IO ()
askImportOptions t ifs cfs m control = do
    let (dialog, grid) = case t of
          ImportFields -> (importFieldsOptionsDialog control, importFieldsOptionsRows control)
          ImportRows -> (importRowsOptionsDialog control, importRowsOptionsRows control)

    #setRowSpacing grid 3
    #setColumnSpacing grid 9
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
            l <- #getLabel btn
            let Just n = elemIndex l options
                n' = (n+1) `mod` length options
            #setLabel btn (options !! n')
        addComboBox grid ifst 2 row

    #showAll dialog
    set dialog [ #transientFor := window $ mainWindow control
               , #modal := True
               , #typeHint := WindowTypeHintDialog
               , #windowPosition := WindowPositionCenterOnParent
               ]
    r <- #run dialog
    #hide dialog

    when (isResponse r ResponseTypeOk) $ do
        l <- catMaybes <$> forM (enumerate cfs) (\(row, _) -> do
            Just b <- #getChildAt grid 1 row
            Just btn <- castTo Button b
            option <- #getLabel btn
            Just cb <- #getChildAt grid 2 row
            Just cbox <- castTo ComboBox cb
            i <- #getActive cbox
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
  dlg <- dialogNew
  set dlg [ #transientFor := window $ mainWindow control
          , #modal := True
          , #typeHint := WindowTypeHintDialog
          , #windowPosition := WindowPositionCenterOnParent
          ]
  content <- #getContentArea dlg
  label <- labelNew $ Just $ if changed
                             then "Ha habido cambios, ¿cómo quieres salir?"
                             else "¿Seguro que quieres salir?"
  #packStart content label True True 8

  if changed
  then do
         #addButton dlg "Grabar y salir" 1
         #addButton dlg "Salir sin grabar" $ asInt32 ResponseTypeYes
         #addButton dlg "No salir" $ asInt32 ResponseTypeNo
  else do
         #addButton dlg "Sí" $ asInt32 ResponseTypeYes
         #addButton dlg "No" $ asInt32 ResponseTypeNo

  #showAll dlg
  r <- #run dlg
  when (isResponse r ResponseTypeYes) $ do
                        sendInput control ExitProgram
                        mainQuit
  when (r == 1) $ do
                   sendInput control WriteFile
                   sendInput control ExitProgram
                   mainQuit
  #destroy dlg

askCreateField :: GUIControl -> IO ()
askCreateField control = do
    dlg <- dialogNew
    set dlg [ #transientFor := window $ mainWindow control
            , #modal := True
            , #typeHint := WindowTypeHintDialog
            , #windowPosition := WindowPositionCenterOnParent
            ]
    #addButton dlg "Crear" $ asInt32 ResponseTypeOk
    #addButton dlg "Cancelar" $ asInt32 ResponseTypeCancel
    content <- #getContentArea dlg
    labelNew (Just "Crear Campos") >>= #add content

    grid <- gridNew
    addLabel grid "Nombre" 0 0
    addLabel grid "Tipo" 1 0
    entries <- forM [1..10] $ \row -> (,)
                                    <$> addEntry grid 0 row
                                    <*> addComboBox grid (map snd typeLabels) 1 row

    #add content grid

    #showAll dlg
    r <- #run dlg

    when (isResponse r ResponseTypeOk) $ do
        fields <- catMaybes <$> forM entries (\(entry, cbox) -> do
                                        name <- #getText entry
                                        i <- #getActive cbox
                                        return $ if T.null name || i == -1
                                                 then Nothing
                                                 else Just (name, fst $ typeLabels !!! i)
                                )
        unless (null fields) $ sendInput control $ NewFields fields
    #destroy dlg

addLabel :: Grid -> Text -> Int32 -> Int32 -> IO Label
addLabel grid text left top = do
    lbl <- labelNew $ Just text
    lbl `set` [ #halign := AlignStart ]
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

askDeleteFields :: [FieldName] -> GUIControl -> IO ()
askDeleteFields names control = do
    dlg <- dialogNew
    set dlg [ #transientFor := window $ mainWindow control
            , #modal := True
            , #typeHint := WindowTypeHintDialog
            , #windowPosition := WindowPositionCenterOnParent
            ]
    #addButton dlg "Borrar" $ asInt32 ResponseTypeOk
    #addButton dlg "Cancelar" $ asInt32 ResponseTypeCancel
    content <- #getContentArea dlg
    labelNew (Just "Borrar Campos") >>= #add content

    grid <- gridNew
    cbuttons <- forM (enumerate names) $ \(row, name) -> addCheckButton grid name 0 row

    #add content grid

    #showAll dlg
    r <- #run dlg

    when (isResponse r ResponseTypeOk) $ do
        fields <- map fst <$> filterM (#getActive . snd) (enumerate cbuttons)
        unless (null fields) $ sendInput control $ DeleteFields fields
    #destroy dlg

askRenameFields :: [FieldName] -> GUIControl -> IO ()
askRenameFields names control = do
    dlg <- dialogNew
    set dlg [ #transientFor := window $ mainWindow control
            , #modal := True
            , #typeHint := WindowTypeHintDialog
            , #windowPosition := WindowPositionCenterOnParent
            ]
    #addButton dlg "Cambiar" $ asInt32 ResponseTypeOk
    #addButton dlg "Cancelar" $ asInt32  ResponseTypeCancel
    content <- #getContentArea dlg
    labelNew (Just "Cambiar Nombres de Campos") >>= #add content

    grid <- gridNew
    #setColumnSpacing grid 4
    centries <- forM (enumerate names) $ \(row, name) -> do
        addLabel grid name 0 row
        entry <- addEntry grid 1 row
        #setText entry name
        return entry

    #packStart content grid True True 8

    #showAll dlg
    r <- #run dlg

    when (isResponse r ResponseTypeOk) $ do
        names <- mapM #getText centries
        sendInput control $ RenameFields names
    #destroy dlg

askSortRows :: [FieldName] -> GUIControl -> IO ()
askSortRows names control = do
    dlg <- dialogNew
    set dlg [ #transientFor := window $ mainWindow control
            , #modal := True
            , #typeHint := WindowTypeHintDialog
            , #windowPosition := WindowPositionCenterOnParent
            ]
    #addButton dlg "Ascendente" 1
    #addButton dlg "Descendente" 2
    #addButton dlg "Cancelar" 3
    content <- #getContentArea dlg
    labelNew (Just "Ordenar") >>= #add content

    grid <- gridNew

    btn <- addRadioButton grid (head names) 0 0
    #setActive btn True
    cbuttons <- (btn :) <$> forM (enumerate $ tail names) (\(row, name) ->
        addRadioButtonFromWidget grid btn name 0 (row + 1))

    #add content grid

    #showAll dlg
    r <- #run dlg

    unless (r == 3) $ do
        fp <- fst . head . filter snd . enumerate <$> mapM #getActive cbuttons
        let order = case r of
                        1 -> Ascending
                        2 -> Descending
        sendInput control $ SortRows fp order
    #destroy dlg


getFieldFormula :: FieldPos -> FieldName -> Maybe Formula -> GUIControl -> IO ()
getFieldFormula fieldPos fieldName mFormula control = do
    let dlg = changeFieldFormulaDialog control
        btn = changeFieldFormulaButton control
        entry = changeFieldFormulaEntry control
        lbl = changeFieldFormulaLabel control
    set dlg [ #transientFor := window $ mainWindow control
            , #modal := True
            , #typeHint := WindowTypeHintDialog
            , #windowPosition := WindowPositionCenterOnParent
            ]

    #setActive btn $ isJust mFormula
    entry `set` [ #text := fromMaybe "" mFormula
                , #sensitive := isJust mFormula
                , #activatesDefault := True
                ]
    #setText lbl $ fieldName `T.append` " = "

    #showAll dlg
    r <- #run dlg
    #hide dlg
    -- putStrLn $ "Response: " ++ show r
    when (isResponse r ResponseTypeOk) $ do
        active <- #getActive btn
        f <- #getText entry
        sendInput control $ ChangeFieldFormula (if active
                                                then Just f
                                                else Nothing) fieldPos

useCombo :: Dialog -> ComboBoxText -> Text -> [Text] -> GUIControl -> IO (Maybe Text)
useCombo dlg combo initial values control = do
    set dlg [ #transientFor := window $ mainWindow control
            , #modal := True
            , #typeHint := WindowTypeHintDialog
            , #windowPosition := WindowPositionCenterOnParent
            ]

    #removeAll combo
    forM_ values $ #appendText combo
    #setActive combo . fromIntegral . fromJust $ elemIndex initial values

    #showAll dlg
    r <- #run dlg
    #hide dlg
    if isResponse r ResponseTypeOk
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
    when (isJust mt) $ setTextField fieldPos (fromJust mt) (mainWindow control)

unimplemented :: String -> GUIControl -> IO ()
unimplemented func control = sendInput control . MessageDialog . ErrorMessage $ T.concat ["Función ", T.pack func, " no implementada"]
