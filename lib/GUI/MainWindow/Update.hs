{-# LANGUAGE OverloadedStrings
           , OverloadedLabels
#-}

module GUI.MainWindow.Update (
  -- *Functions
  changeTitle
  , updatePosition
  , showFields
  , updateNames
  , disableTextViews
  , setTextField
) where

import Control.Concurrent.Chan(writeChan)
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
import Model hiding (deleteFields)
import Model.DefaultFileNames
import Presenter.ImportType
import Presenter.Input

changeTitle :: Text -> MainWindow -> IO ()
changeTitle title mWindow = set (window mWindow) [ windowTitle := title ]

updatePosition :: Int -> Int -> MainWindow -> IO ()
updatePosition pos size mWindow = do
    beginButton mWindow `set` [ #sensitive := pos > 1 ]
    leftButton mWindow `set` [ #sensitive := pos > 1 ]
    endButton mWindow `set` [ #sensitive := pos < size ]
    rightButton mWindow `set` [ #sensitive := pos < size ]

    labelSetText (positionLabel mWindow) $ T.concat [showt pos, "/", showt size]

enumerate :: Integral int => [a] -> [(int, a)]
enumerate = zip [0..]

showFields :: [FieldInfo] -> MainWindow -> IO ()
showFields fis mWindow = do
  let grid = fieldsGrid mWindow

  forM_ fis $ \fi -> do
                       textView <- recoverTextView (indexFI fi) mWindow
                       disconnectTextView (indexFI fi) mWindow
                       let tooltip = fromMaybe (typeLabel $ typeFI fi) $ formulaFI fi
                       label <- recoverLabel (indexFI fi) mWindow
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
                       reconnectTextView (indexFI fi) mWindow
  widgetShowAll grid

recoverTextView :: FieldPos -> MainWindow -> IO TextView
recoverTextView row mWindow = do
    -- putStrLn $ "Recovering textView in row " ++ show row
    Just tv <- gridGetChildAt (fieldsGrid mWindow) 1 row
    unsafeCastTo TextView tv

recoverLabel :: FieldPos -> MainWindow -> IO Label
recoverLabel row mWindow = do
    -- putStrLn $ "Recovering label in row " ++ show row
    Just ebox <- gridGetChildAt (fieldsGrid mWindow) 0 row
    cbox <- unsafeCastTo EventBox ebox
    lbl <- head <$> containerGetChildren cbox
    unsafeCastTo Label lbl

addTextBufferActive :: Int -> MainWindow -> IO ()
addTextBufferActive n mWindow = modifyIORef (textBufferActive mWindow) (\bv -> ones n # bv)

deleteTextBufferActive :: [FieldPos] -> MainWindow -> IO ()
deleteTextBufferActive ps mWindow = let
    survivors :: Int32 -> [Int32] -> BV -> BV
    survivors l [] bv = add l (fromIntegral $ BV.size bv - 1) bv nil
    survivors l (p:ps) bv = add l (p-1) bv $ survivors (p+1) ps bv
    add :: Int32 -> Int32 -> BV -> BV -> BV
    add l r bv | l <= r = (extract r l bv #)
               | otherwise = id
  in modifyIORef (textBufferActive mWindow) (survivors 0 ps)

disableTextViews :: MainWindow -> IO ()
disableTextViews mWindow = do
  nfields <- readIORef $ numberOfFields mWindow
  forM_ [0 .. fromIntegral nfields-1] $ \f -> do
                                textView <- recoverTextView f mWindow
                                set textView [ #editable := False
                                             , #canFocus := False
                                             , #sensitive := False
                                             , #name := "empty"
                                             ]

updateNames :: [FieldName] -> MainWindow -> IO ()
updateNames names mWindow = do
  let grid = fieldsGrid mWindow
  adjustTextFields (length names) mWindow

  forM_ (enumerate names) $ \(r, name) -> do
                             label <- recoverLabel r mWindow
                             labelSetText label name
  widgetShowAll grid

adjustTextFields :: Int -> MainWindow -> IO ()
adjustTextFields nfields mWindow = do
  let grid = fieldsGrid mWindow
  current <- readIORef $ numberOfFields mWindow
  case compare current nfields of
    LT -> addFields grid [fromIntegral current .. fromIntegral nfields - 1] mWindow
    EQ -> return ()
    GT -> deleteFields grid [fromIntegral nfields .. fromIntegral current - 1] mWindow
  writeIORef (numberOfFields mWindow) nfields

addFields :: Grid -> [FieldPos] -> MainWindow -> IO ()
addFields grid fields mWindow = do
    forM_ fields $ \f -> do
       lbl <- createFieldLabel f mWindow
       #attach grid lbl 0 f 1 1
       textView <- createFieldTextView f mWindow
       #attach grid textView 1 f 1 1
    addTextBufferActive (length fields) mWindow

dndError :: MainWindow -> IO ()
dndError mWindow = sendInputMW mWindow $ MessageDialog (ErrorMessage "Algo está mal en el dnd")

sendInputMW :: IsInput cmd => MainWindow -> cmd -> IO ()
sendInputMW mWindow = writeChan (inputChanMW mWindow) . toInput

createFieldLabel :: FieldPos -> MainWindow -> IO EventBox
createFieldLabel f mWindow = do
         lbl <- labelNew $ Just ""
         #setHalign lbl AlignStart
         ebox <- eventBoxNew
         #dragSourceSet ebox [ModifierTypeButton1Mask] Nothing [DragActionMove] -- Check the Nothing, I have no idea if it is correct
         #dragSourceSetTargetList ebox (Just $ targetList mWindow)
         #dragDestSet ebox [DestDefaultsAll] Nothing [DragActionMove] -- Ditto for Nothing
         #dragDestSetTargetList ebox (Just $ targetList mWindow)
         ebox `on` #dragDataGet $ \_ sdata _ _ -> do
                                   let (t,l) = (showt f, fromIntegral $ T.length t)
                                   ok <- selectionDataSetText sdata t l
                                   unless ok (liftIO $ dndError mWindow)
         ebox `on` #dragDataReceived $ \_ _ _ sdata _ _ -> do
                               t <- selectionDataGetText sdata
                               liftIO $ case (t :: Maybe Text) of
                                          Nothing -> dndError mWindow
                                          Just v -> let
                                                      from = read $ T.unpack v
                                                    in when (from /= f)
                                                            (sendInputMW mWindow $ MoveField from f)

         #add ebox lbl
         return ebox

clearBit' :: BV.BitVector -> Int -> BV.BitVector
clearBit' bv = complement . setBit (complement bv)

disconnectTextView :: FieldPos -> MainWindow -> IO ()
disconnectTextView = changeActiveStatus clearBit'

reconnectTextView :: FieldPos -> MainWindow -> IO ()
reconnectTextView = changeActiveStatus setBit

changeActiveStatus :: (BV.BitVector -> Int -> BV.BitVector) ->  FieldPos -> MainWindow -> IO ()
changeActiveStatus f t mWindow = modifyIORef (textBufferActive mWindow) (flip f $ fromIntegral t)

createFieldTextView :: FieldPos -> MainWindow -> IO TextView
createFieldTextView f mWindow = do
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
             isActive <- (@. f) <$> readIORef (textBufferActive mWindow)
             when isActive $ do
                 begin <- #getStartIter buffer
                 end <- #getEndIter buffer
                 text <- #getText buffer begin end False
                 sendInputMW mWindow $ UpdateField f (toField text)

         textView `on` #buttonPressEvent $ \event -> do
             button <- get event #button
             if button == 3
             then liftIO $ do
                      writeIORef (currentField mWindow) f
                      #popupAtPointer (fieldMenu mWindow) Nothing
                      return True
             else return False
         return textView

deleteFields :: Grid -> [FieldPos] -> MainWindow -> IO ()
deleteFields grid fields mWindow = do
                      forM_ fields $ \f ->
                         forM_ [0, 1] $ \c -> do
                             Just w <- #getChildAt grid c f
                             #destroy w
                      deleteTextBufferActive fields mWindow

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

setTextField :: FieldPos -> Text -> MainWindow -> IO ()
setTextField fieldPos t mWindow = do
  textView <- recoverTextView fieldPos mWindow
  editable <- #getEditable textView
  when editable $ do
    buffer <- #getBuffer textView
    #setText buffer t (fromIntegral $ T.length t)

unimplemented :: String -> GUIControl -> IO ()
unimplemented func control = sendInput control . MessageDialog . ErrorMessage $ T.concat ["Función ", T.pack func, " no implementada"]
