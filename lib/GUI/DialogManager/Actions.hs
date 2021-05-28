{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings
           , OverloadedLabels
#-}

module GUI.DialogManager.Actions (
  -- *Types
  DialogFunction
  , DialogAction
  -- *Functions
  , displayMessage
  , askReadFile
  , askWriteFile
  , askFile
  , askImportFrom
  , askImportOptions
  , confirmExit
  , askCreateField
  , askDeleteFields
  , askRenameFields
  , askRenameSources
  , askSortRows
  , getFieldFormula
  , searchField
  , copyOther
  , showSources
  , showAboutDialog
) where

import Control.Monad(filterM, forM, forM_, unless, when)
import Data.Either(lefts, rights)
import Data.Functor((<&>))
import Data.GI.Base.Attributes(clear)
import Data.GI.Base.GType(gtypeString)
import Data.List(elemIndex)
import Data.Maybe(catMaybes, fromJust, fromMaybe, isJust)
import Data.Text(Text)
import qualified Data.Text as T
import GHC.Int(Int32)
import GI.Gtk hiding (MessageDialog)
import GI.Gdk hiding (Window)


import GUI.Command
import GUI.DialogManager
import Model hiding (deleteFields)
import Model.DefaultFileNames
import Model.SourceInfo
import Presenter.ImportType

displayMessage :: Message -> Window -> IO ()
displayMessage (ErrorMessage m) = noResponseMessage m
displayMessage (WarningMessage m) = noResponseMessage m
displayMessage (InformationMessage m) = noResponseMessage m
displayMessage (QuestionMessage _) = undefined

createDialog :: Window -> IO Dialog
createDialog parent = do
  dlg <- dialogNew
  configureDialog parent dlg
  return dlg


createDialogButtons :: Window -> [(Text, Int32)] -> IO Dialog
createDialogButtons parent btns = do
  dlg <- createDialog parent
  forM_ btns $ uncurry (#addButton dlg)
  return dlg


createDialogButtonsLabel :: Window -> [(Text, Int32)] -> Text -> IO Dialog
createDialogButtonsLabel parent btns lbl = do
  dlg <- createDialogButtons parent btns
  content <- #getContentArea dlg
  labelNew (Just lbl) >>= #add content
  return dlg


configureDialog :: Window -> Dialog -> IO ()
configureDialog parent dlg = do
  set dlg [ #transientFor := parent
          , #modal := True
          , #typeHint := WindowTypeHintDialog
          , #windowPosition := WindowPositionCenterOnParent
          ]

showAndRun :: IsDialog dlg => dlg -> IO Int32
showAndRun dl = do
    dlg <- toDialog dl
    #showAll dlg
    #run dlg

showRunAndDestroy :: IsDialog dlg => dlg -> IO Int32
showRunAndDestroy dl = do
    dlg <- toDialog dl
    #showAll dlg
    r <- #run dlg
    #destroy dlg
    return r

showRunAndHide :: IsDialog dlg => dlg -> IO Int32
showRunAndHide dl = do
  dlg <- toDialog dl
  #showAll dlg
  runAndHide dlg

runAndHide :: IsDialog dlg => dlg -> IO Int32
runAndHide dl = do
  dlg <- toDialog dl
  r <- #run dlg
  #hide dlg
  return r

noResponseMessage :: Text -> Window -> IO ()
noResponseMessage m parent = do
    dlg <- createDialogButtons parent [("Ok", asInt32 ResponseTypeOk)]
    content <- #getContentArea dlg
    message <- labelNew $ Just m
    #packStart content message True True 2
    _ <- showAndRun dlg
    #destroy dlg
    return ()

enumerate :: Integral int => [a] -> [(int, a)]
enumerate = zip [0..]

type DialogAction info = info -> IO ()

type DialogFunction info = DialogManager -> DialogAction info -> Window -> IO ()

askReadFile :: DialogFunction PathAndConf
askReadFile dmg = askFile (loadFileDialog dmg)
                          (confFileLoadCheckButton dmg)

askWriteFile :: DialogFunction PathAndConf
askWriteFile dmg = askFile (saveAsDialog dmg)
                           (confFileSaveCheckButton dmg)

isResponse :: Int32 -> ResponseType -> Bool
isResponse r = (== r) . fromIntegral . fromEnum

asInt32 :: Enum e => e -> Int32
asInt32 = fromIntegral . fromEnum

askFile :: FileChooserDialog
        -> CheckButton
        -> DialogAction PathAndConf
        -> Window
        -> IO ()
askFile dialog button action parent = do
    configureDialog parent =<< toDialog dialog
    r <- runAndHide dialog
    when (isResponse r ResponseTypeOk) $ do
            file <- #getFilename dialog
            when (isJust file) $ do
                chk <- #getActive button
                let fp = fromJust file
                    conf = if chk
                           then defaultConfFileName <$> file
                           else Nothing
                action (PathAndConf fp conf)

askImportFrom :: DialogFunction (FilePath, Char, HeaderType)
askImportFrom dmg action parent = do
    let dialog = importFromFileDialog dmg
    configureDialog parent =<< toDialog dialog
    r <- runAndHide dialog
    when (isResponse r ResponseTypeOk) $ do
        file <- #getFilename dialog
        separator <- translateChar <$> #getText (importInputSeparator dmg)
        header <- #getActiveText (importInputFormat dmg) <&> (\case
                      Just "Comentario" -> Comment
                      Just "Sin cabecera" -> NoHeader
                      Just "Primera línea" -> FirstLine
                      _ -> Comment -- TODO: deal with the possibility of an error
                      )
        when (isJust file) $ action (fromJust file, separator, header)

askImportOptions :: ImportType -> [FieldName] -> [FieldName] -> DialogFunction ([(FieldPos, FieldPos)], [(FieldPos, FieldPos)])
askImportOptions t ifs cfs dmg action parent = do
    let (dialog, grid) = case t of
          ImportFields -> (importFieldsOptionsDialog dmg, importFieldsOptionsRows dmg)
          ImportRows -> (importRowsOptionsDialog dmg, importRowsOptionsRows dmg)

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
        _ <- btn `on` #clicked $ do
            l <- #getLabel btn
            let Just n = elemIndex l options
                n' = (n+1) `mod` length options
            #setLabel btn (options !! n')
        addComboBox grid ifst 2 row

    configureDialog parent dialog
    r <- showRunAndHide dialog

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
        action (keys, values)

translateChar :: Text -> Char
translateChar t = case T.unpack t of
                     "" -> '\t'
                     [c] -> c
                     '\\': 't' : _ -> '\t'
                     '\\': c : _ -> c
                     c: _ -> c

confirmExit :: Bool -> DialogFunction Bool
confirmExit changed _ action parent = do
  let buttons = if changed
                then [("Grabar y salir", 1)
                     ,("Salir sin grabar", asInt32 ResponseTypeYes)
                     ,("No salir", asInt32 ResponseTypeNo)]
                else [("Sí", asInt32 ResponseTypeYes)
                     ,("No", asInt32 ResponseTypeNo)]
  dlg <- createDialogButtons parent buttons
  content <- #getContentArea dlg
  label <- labelNew $ Just $ if changed
                             then "Ha habido cambios, ¿cómo quieres salir?"
                             else "¿Seguro que quieres salir?"
  #packStart content label True True 8

  r <- showRunAndDestroy dlg
  when (isResponse r ResponseTypeYes) $ action False
  when (r == 1) $ action True

askCreateField :: DialogFunction [(FieldName, FieldType)]
askCreateField _ action parent = do
    dlg <- createDialogButtonsLabel parent
                         [("Crear", asInt32 ResponseTypeOk)
                         ,("Cancelar", asInt32 ResponseTypeCancel)]
                         "Crear Campos"
    content <- #getContentArea dlg

    grid <- gridNew
    _ <- addLabel grid "Nombre" 0 0
    _ <- addLabel grid "Tipo" 1 0
    entries <- forM [1..10] $ \row -> (,)
                                    <$> addEntry grid 0 row
                                    <*> addComboBox grid (map snd typeLabels) 1 row

    #add content grid

    r <- showAndRun dlg

    when (isResponse r ResponseTypeOk) $ do
        fields <- catMaybes <$> forM entries (\(entry, cbox) -> do
                                        name <- #getText entry
                                        i <- #getActive cbox
                                        return $ if T.null name || i == -1
                                                 then Nothing
                                                 else Just (name, fst $ typeLabels !!! i)
                                )
        unless (null fields) $ action fields

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

askDeleteFields :: [FieldName] -> DialogFunction [FieldPos]
askDeleteFields names _ action parent = do
    dlg <- createDialogButtonsLabel parent
                      [("Borrar", asInt32 ResponseTypeOk)
                      ,("Cancelar", asInt32 ResponseTypeCancel)]
                      "Borrar Campos"

    grid <- gridNew
    cbuttons <- forM (enumerate names) $ \(row, name) -> addCheckButton grid name 0 row

    content <- #getContentArea dlg
    #add content grid

    r <- showAndRun dlg

    when (isResponse r ResponseTypeOk) $ do
        fields <- map fst <$> filterM (#getActive . snd) (enumerate cbuttons)
        unless (null fields) $ action fields

    #destroy dlg


askRename :: Text -> [Text] -> DialogFunction [Text]
askRename header labels _ action parent = do
    dlg <- createDialogButtonsLabel parent
               [("Cambiar", asInt32 ResponseTypeOk)
               ,("Cancelar", asInt32  ResponseTypeCancel)]
               header

    grid <- gridNew
    #setColumnSpacing grid 4
    centries <- forM (enumerate labels) $ \(rw, label) -> do
        _ <- addLabel grid label 0 rw
        entry <- addEntry grid 1 rw
        #setText entry label
        return entry

    content <- #getContentArea dlg
    #packStart content grid True True 8

    r <- showAndRun dlg

    when (isResponse r ResponseTypeOk)
         (mapM #getText centries >>= action)

    #destroy dlg


askRenameFields :: [FieldName] -> DialogFunction [FieldName]
askRenameFields = askRename "Cambiar nombres de campos"


askRenameSources :: [SourceName] -> DialogFunction [SourceName]
askRenameSources = askRename "Cambiar nombres de fuentes"


askSortRows :: [FieldName] -> DialogFunction (FieldPos, SortDirection)
askSortRows names dmg action parent = do
    dlg <- createDialogButtonsLabel parent
                 [("Ascendente", 1)
                 ,("Descendente", 2)
                 ,("Cancelar", 3)]
                 "Ordenar"

    grid <- gridNew

    btn <- addRadioButton grid (head names) 0 0
    #setActive btn True
    cbuttons <- (btn :) <$> forM (enumerate $ tail names) (\(row, name) ->
        addRadioButtonFromWidget grid btn name 0 (row + 1))

    content <- #getContentArea dlg
    #add content grid

    r <- showAndRun dlg

    unless (r == 3) $ do
        fp <- fst . head . filter snd . enumerate <$> mapM #getActive cbuttons
        let order = case r of
                        1 -> Ascending
                        2 -> Descending
        action (fp, order)

    #destroy dlg

getFieldFormula :: FieldPos -> FieldName -> Maybe Formula -> DialogFunction (Maybe Formula)
getFieldFormula _ fieldName mFormula dmg action parent = do
    let dlg = changeFieldFormulaDialog dmg
        btn = changeFieldFormulaButton dmg
        entry = changeFieldFormulaEntry dmg
        lbl = changeFieldFormulaLabel dmg
    configureDialog parent dlg

    buffer <- textViewGetBuffer entry
    buffer `set` [ #text := fromMaybe "" mFormula ]
    #setActive btn $ isJust mFormula
    entry `set` [ #sensitive := isJust mFormula ]
    #setText lbl $ fieldName `T.append` " = "
    _ <- entry `on` #keyPressEvent $ \evk -> do
      n <- get evk #keyval >>= keyvalName
      case n of
          Just "Return" -> return True
          _ -> return False

    r <- showRunAndHide dlg
    when (isResponse r ResponseTypeOk) $ do
        active <- #getActive btn
        f <- do
               begin <- #getStartIter buffer
               end <- #getEndIter buffer
               #getText buffer begin end False
        action $ if active
                 then Just f
                 else Nothing

useCombo :: Dialog -> ComboBoxText -> Text -> [Text] -> DialogManager -> Window -> IO (Maybe Text)
useCombo dlg combo initial values _ parent = do
    configureDialog parent dlg

    #removeAll combo
    forM_ values $ #appendText combo
    #setActive combo . fromIntegral . fromJust $ elemIndex initial values

    r <- showRunAndHide dlg
    if isResponse r ResponseTypeOk
    then #getActiveText combo
    else return Nothing

searchField :: FieldPos -> Text -> [Text] -> DialogFunction Text
searchField _ initial values dmg action parent = do
    let dlg = searchFieldDialog dmg
        combo = searchFieldCombo dmg

    mt <- useCombo dlg combo initial values dmg parent
    forM_ mt action

copyOther :: FieldPos -> Text -> [Text] -> DialogFunction Text
copyOther _ initial values dmg action parent = do
    let dlg = copyOtherDialog dmg
        combo = copyOtherCombo dmg

    mt <- useCombo dlg combo initial values dmg parent
    forM_ mt action


showSources :: DialogManager -> [(RowStoreName, FilePath, [FieldName])] -> Window -> IO ()
showSources dmg srcs parent = do
    let dlg = showSourcesDialog dmg
        trv = sourcesTreeView dmg
    configureDialog parent dlg
    fillTreeView trv srcs
    showRunAndHide dlg
    return ()

fillTreeView :: TreeView -> [(RowStoreName, FilePath, [FieldName])] -> IO ()
fillTreeView tv srcs = do
    let types = [gtypeString]
    ts <- treeStoreNew types
    current <- #getNColumns tv
    unless (current == 1) $ do
        col <- treeViewColumnNew
        renderer <- cellRendererTextNew
        #packStart col renderer True
        #addAttribute col renderer "text" 0
        #setTitle col "Fuentes"
        #appendColumn tv col
        return ()
    let srcs' = if null srcs
                then [("No hay fuentes", "", [])]
                else srcs
    forM_ srcs' $ \(src, fp, names) -> do
        it <- #append ts Nothing
        v <- toGValue $ Just (T.concat [src, " (", T.pack fp, ")" ])
        #setValue ts it 0 v
        forM_ names $ \name -> do
           it' <- #append ts $ Just it
           v <- toGValue $ Just name
           #setValue ts it' 0 v
    clear tv #model
    #setModel tv $ Just ts


showAboutDialog :: DialogManager -> Window -> IO ()
showAboutDialog dmg parent = do
    let dlg = aboutDialog dmg
    configureDialog parent dlg
    runAndHide dlg
    return ()

