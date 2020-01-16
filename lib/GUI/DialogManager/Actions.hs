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
  , askSortRows
  , getFieldFormula
  , searchField
  , copyOther
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
import GI.Gdk hiding (Window)
import TextShow(TextShow(showt))

import GUI.Command
import GUI.DialogManager
import Model hiding (deleteFields)
import Model.DefaultFileNames
import Presenter.ImportType
import Presenter.Input

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

configureDialog :: IsDialog dlg => Window -> dlg -> IO ()
configureDialog parent dlg = do
  dlg <- toDialog dlg
  set dlg [ #transientFor := parent
          , #modal := True
          , #typeHint := WindowTypeHintDialog
          , #windowPosition := WindowPositionCenterOnParent
          ]

showAndRun :: IsDialog dlg => dlg -> IO Int32
showAndRun dlg = do
    dlg <- toDialog dlg
    #showAll dlg
    #run dlg

showRunAndDestroy :: IsDialog dlg => dlg -> IO Int32
showRunAndDestroy dlg = do
    dlg <- toDialog dlg
    #showAll dlg
    r <- #run dlg
    #destroy dlg
    return r

showRunAndHide :: IsDialog dlg => dlg -> IO Int32
showRunAndHide dlg = do
  dlg <- toDialog dlg
  #showAll dlg
  runAndHide dlg

runAndHide :: IsDialog dlg => dlg -> IO Int32
runAndHide dlg = do
  dlg <- toDialog dlg
  r <- #run dlg
  #hide dlg
  return r

noResponseMessage :: Text -> Window -> IO ()
noResponseMessage m parent = do
    dlg <- createDialog parent
    #addButton dlg "Ok" $ asInt32 ResponseTypeOk
    content <- #getContentArea dlg
    message <- labelNew $ Just m
    #packStart content message True True 2
    showAndRun dlg
    #destroy dlg
    return ()

enumerate :: Integral int => [a] -> [(int, a)]
enumerate = zip [0..]

type DialogAction info = info -> IO ()

type DialogFunction info = DialogManager -> DialogAction info -> Window -> IO ()

askReadFile :: DialogFunction (FilePath, Maybe FilePath)
askReadFile dmg = askFile (loadFileDialog dmg)
                          (confFileLoadCheckButton dmg)

askWriteFile :: DialogFunction (FilePath, Maybe FilePath)
askWriteFile dmg = askFile (saveAsDialog dmg)
                           (confFileSaveCheckButton dmg)

isResponse :: Int32 -> ResponseType -> Bool
isResponse r = (== r) . fromIntegral . fromEnum

asInt32 :: Enum e => e -> Int32
asInt32 = fromIntegral . fromEnum

askFile :: FileChooserDialog
        -> CheckButton
        -> DialogAction (FilePath, Maybe FilePath)
        -> Window
        -> IO ()
askFile dialog button action parent = do
    configureDialog parent dialog
    r <- runAndHide dialog
    when (isResponse r ResponseTypeOk) $ do
            file <- #getFilename dialog
            when (isJust file) $ do
                chk <- #getActive button
                let fp = fromJust file
                    conf = if chk
                           then defaultConfFileName <$> file
                           else Nothing
                action (fp, conf)

askImportFrom :: DialogFunction (FilePath, Char)
askImportFrom dmg action parent = do
    let dialog = importFromFileDialog dmg
    configureDialog parent dialog
    r <- runAndHide dialog
    when (isResponse r ResponseTypeOk) $ do
        file <- #getFilename dialog
        separator <- translateChar <$> #getText (importInputSeparator dmg)
        when (isJust file) $ action (fromJust file, separator)

askImportOptions :: ImportType -> [FieldName] -> [FieldName] -> Model -> DialogFunction ([(FieldPos, FieldPos)], [(FieldPos, FieldPos)])
askImportOptions t ifs cfs m dmg action parent = do
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
        btn `on` #activate $ do
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
translateChar t = case T.uncons t of
                    Nothing -> '\t'
                    Just ('\\', t') -> if T.null t'
                                       then '\\'
                                       else T.head t'
                    Just (c, _) -> c

confirmExit :: Bool -> DialogFunction Bool
confirmExit changed dmg action parent = do
  dlg <- createDialog parent
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

  r <- showRunAndDestroy dlg
  when (isResponse r ResponseTypeYes) $ action False
  when (r == 1) $ action True

askCreateField :: DialogFunction [(FieldName, FieldType)]
askCreateField dmg action parent = do
    dlg <- createDialog parent

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
askDeleteFields names dmg action parent = do
    dlg <- createDialog parent

    #addButton dlg "Borrar" $ asInt32 ResponseTypeOk
    #addButton dlg "Cancelar" $ asInt32 ResponseTypeCancel
    content <- #getContentArea dlg
    labelNew (Just "Borrar Campos") >>= #add content

    grid <- gridNew
    cbuttons <- forM (enumerate names) $ \(row, name) -> addCheckButton grid name 0 row

    #add content grid

    r <- showAndRun dlg

    when (isResponse r ResponseTypeOk) $ do
        fields <- map fst <$> filterM (#getActive . snd) (enumerate cbuttons)
        unless (null fields) $ action fields

    #destroy dlg

askRenameFields :: [FieldName] -> DialogFunction [FieldName]
askRenameFields names dmg action parent = do
    dlg <- createDialog parent
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

    r <- showAndRun dlg

    when (isResponse r ResponseTypeOk)
         (mapM #getText centries >>= action)

    #destroy dlg

askSortRows :: [FieldName] -> DialogFunction (FieldPos, SortDirection)
askSortRows names dmg action parent = do
    dlg <- createDialog parent
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

    r <- showAndRun dlg

    unless (r == 3) $ do
        fp <- fst . head . filter snd . enumerate <$> mapM #getActive cbuttons
        let order = case r of
                        1 -> Ascending
                        2 -> Descending
        action (fp, order)

    #destroy dlg

getFieldFormula :: FieldPos -> FieldName -> Maybe Formula -> DialogFunction (Maybe Formula)
getFieldFormula fieldPos fieldName mFormula dmg action parent = do
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
    entry `on` #keyPressEvent $ \evk -> do
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
useCombo dlg combo initial values dmg parent = do
    configureDialog parent dlg

    #removeAll combo
    forM_ values $ #appendText combo
    #setActive combo . fromIntegral . fromJust $ elemIndex initial values

    r <- showRunAndHide dlg
    if isResponse r ResponseTypeOk
    then Just <$> #getActiveText combo
    else return Nothing

searchField :: FieldPos -> Text -> [Text] -> DialogFunction Text
searchField fieldPos initial values dmg action parent = do
    let dlg = searchFieldDialog dmg
        combo = searchFieldCombo dmg

    mt <- useCombo dlg combo initial values dmg parent
    forM_ mt action

copyOther :: FieldPos -> Text -> [Text] -> DialogFunction Text
copyOther fieldPos initial values dmg action parent = do
    let dlg = copyOtherDialog dmg
        combo = copyOtherCombo dmg

    mt <- useCombo dlg combo initial values dmg parent
    forM_ mt action


