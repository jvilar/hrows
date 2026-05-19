{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TUI.FieldPropertiesDialog (
    FieldPropertiesDialog
  , FieldPropertiesInfo
  , fpiName
  , fpiType
  , fpiFormulaOrValue
  , mkFieldPropertiesDialog
  , renderFieldPropertiesDialog
  , updateFieldPropertiesDialog
  , handleEventFieldPropertiesDialog
  ) where



import Brick ( Widget (Widget), txt, withAttr, (<=>), (<+>), getContext, Size (..), vSize, availWidthL, render, str, clickable, EventM, BrickEvent (VtyEvent, MouseDown) )
import Brick qualified as B
import Brick.Widgets.Border ( hBorder )
import Brick.Widgets.Dialog ( Dialog, dialog, renderDialog, dialogWidthL, setDialogFocus )
import Brick.Widgets.Edit qualified as Ed
import Control.Lens ( makeLenses, over, (^.), lens, set, view, (.=), use, _Left )
import Control.Monad.State (get, modify)
import Data.Maybe(fromMaybe, isJust)
import Data.Text (Text)
import Graphics.Vty (Event(EvKey), Button (..), Key (..), Modifier)

import Model.Expression.Evaluation (evaluate)
import Model.Expression.Manipulate (addPositions)
import Model.Expression.Parser (parseExpression)
import Model.RowStore
import TUI.Base

data FieldPropertiesFocus = FpName | FpValue | FpType | FpFMark | FpFormula | FpOK | FpCancel deriving (Eq, Ord, Enum, Bounded)

data FieldPropertiesInfo = FieldPropertiesInfo { _fpiName :: Text
                                               , _fpiType :: FieldType
                                               , _fpiFormulaOrValue :: Either Formula Field
                                               }

makeLenses ''FieldPropertiesInfo

instance Semigroup FieldPropertiesInfo where
    a <> _ = a

data FieldPropertiesDialog = FieldPropertiesDialog { _fpTitle :: Text
                                     , _fpName :: ValueEditor
                                     , _fpValue :: ValueViewer
                                     , _fpType :: FieldType
                                     , _fpIsFormula :: Bool
                                     , _fpFormula :: ValueEditor
                                     , _fpFocus :: FieldPropertiesFocus
                                     , _fpDialog :: Dialog () Name
                                     , _fpRst :: RowStore
                                     , _fpIndex :: Int
                                     }


makeLenses ''FieldPropertiesDialog

instance HasEditor FieldPropertiesDialog where
    editorLens = lens getter setter
        where getter fp = case fp ^. fpFocus of
                              FpName -> Just $ fp ^. fpName
                              FpValue -> case fp ^. fpValue of
                                             Left ve -> Just ve
                                             _ -> Nothing
                              FpFormula -> Just $ fp ^. fpFormula
                              _ -> Nothing
              setter _ Nothing = error "Cannot remove editor from a field properties dialog"
              setter fp (Just ve) = case fp ^. fpFocus of
                                      FpName -> set fpName ve fp
                                      FpValue -> set fpValue (Left ve) fp
                                      FpFormula -> set fpFormula ve fp
                                      _ -> error "Bad focus in field properties dialog when setting editor"

mkFieldPropertiesDialog :: Text -> Bool -> Field -> FieldType -> Maybe Formula -> RowStore -> Int -> FieldPropertiesDialog
mkFieldPropertiesDialog fname isFormula field fType mFormula rst index = FieldPropertiesDialog fname
                            (mkEditor FieldPropertiesNameEditor $ toField fname)
                            (if isFormula
                             then Right field
                             else Left (mkEditor FieldPropertiesValueEditor field)
                             )
                            fType
                            (isJust mFormula)
                            (mkEditor FieldPropertiesFormulaEditor $ toField $ fromMaybe "" mFormula)
                            FpValue
                            (dialog (Just $ txt fname)
                                    (Just (DButton OkButton, [ ("OK", DButton OkButton, ())
                                                             , ("Cancel", DButton CancelButton, ())]))
                                    400)
                            rst
                            index

renderResizeDialog :: Ord n => Int -> Dialog a n -> Widget n -> Widget n
renderResizeDialog pWidth d w = let
    p = renderDialog d w
  in Widget Greedy (vSize p) $ do
    context <- getContext
    let uw = context ^. availWidthL
        dwidth = round (toRational uw * (toRational pWidth / 100))
    render $ renderDialog (set dialogWidthL dwidth d) w

renderFieldPropertiesDialog :: FieldPropertiesDialog -> Widget Name
renderFieldPropertiesDialog fp = renderResizeDialog 95 (fp ^. fpDialog) $ do
                         (clickable FieldPropertiesNameEditor $ txt "Name: " <+> wFocus FpName (renderValueEditor (fp ^. fpName)))
                         <=>
                         hBorder
                         <=>
                         (clickable FieldPropertiesValueEditor $ txt "Value: " <+> renderValueViewer (fp ^. fpValue))
                         <=>
                         hBorder
                         <=>
                         (clickable FieldPropertiesTypeSelector $ txt "Type: " <+> wFocus FpType (str . show $ fp ^. fpType))
                         <=>
                         hBorder
                         <=>
                         ( (clickable FieldPropertiesFormulaMark $ if fp ^. fpIsFormula
                           then wFocus FpFMark (txt "[X]")
                           else wFocus FpFMark (txt "[ ]"))
                           <+>
                           (clickable FieldPropertiesFormulaEditor $ txt " Formula: "
                             <+>
                             renderValueEditor (fp ^. fpFormula)
                           )
                         )
                         <=>
                         hBorder
            where wFocus n = if fp ^. fpFocus == n
                      then withAttr selectedElementAttr
                      else id

handleEventFieldPropertiesDialog :: BrickEvent Name e -> EventM Name FieldPropertiesDialog (DialogEventResult FieldPropertiesInfo)
handleEventFieldPropertiesDialog (VtyEvent (EvKey k ms)) = handleKey k ms
handleEventFieldPropertiesDialog (MouseDown n BLeft [] _) = case n of
    DButton OkButton -> returnFieldProperties
    DButton CancelButton -> return DialogCancel
    FieldPropertiesNameEditor -> changeFocus FpName
    FieldPropertiesValueEditor -> changeFocus FpValue
    FieldPropertiesTypeSelector -> changeFocus FpType
    FieldPropertiesFormulaMark -> switchIsFormula >> changeFocus FpFMark
    FieldPropertiesFormulaEditor -> changeFocus FpFormula
    _ -> return DoNothing
handleEventFieldPropertiesDialog _ = return DoNothing

type FPMonad = EventM Name FieldPropertiesDialog (DialogEventResult FieldPropertiesInfo)

whenFPM :: Bool -> FPMonad -> FPMonad
whenFPM cond action = if cond then action else return DoNothing

handleKey :: Key -> [Modifier] -> FPMonad
handleKey KUp [] = onDialog moveUp
handleKey KDown [] = onDialog moveDown
handleKey (KChar '\t') [] = onDialog moveDown
handleKey KBackTab [] = onDialog moveUp
handleKey KEsc [] = return DialogCancel
handleKey KEnter [] = use fpFocus >>= \case
        FpCancel -> return DialogCancel
        _ -> returnFieldProperties
handleKey k ms = use fpFocus >>= \case
         FpType -> case (k, ms) of
                      (KLeft, []) -> onDialog prevType
                      (KRight, []) -> onDialog nextType
                      _ -> return DoNothing
         FpOK -> whenFPM (k ==  KChar ' ' && null ms) returnFieldProperties
         FpCancel -> whenFPM (k == KChar ' ' && null ms) $ return DialogCancel
         FpName -> B.zoom (fpName . veEditor) (Ed.handleEditorEvent (VtyEvent (EvKey k ms))) >> return DoNothing
         FpValue -> handleEventValueEditor (VtyEvent (EvKey k ms))
         FpFMark -> whenFPM (k == KChar ' ' && null ms) switchIsFormula
         FpFormula -> handleKeyInFormula k ms

switchIsFormula :: FPMonad
switchIsFormula = do
    f <- use $ fpValue . vvValue
    use fpIsFormula >>= \case
        True -> do
                  fpIsFormula .= False
                  fpValue .= Left (mkEditor FieldPropertiesValueEditor f)
        False -> do
                   fpIsFormula .= True
                   fpValue .= Right f
    return DoNothing

handleEventValueEditor :: BrickEvent Name a -> FPMonad
handleEventValueEditor ev = use fpValue >>= \case
    Left _ -> B.zoom (fpValue . _Left ) $ do
                 B.zoom veEditor $ Ed.handleEditorEvent ev
                 ve <- get
                 let f = convertKeepText (ve ^. veType) (toField $ ve ^. veContent)
                 modify $ updateEditor f
                 return DoNothing
    Right _ -> return DoNothing

handleKeyInFormula :: Key -> [Modifier] -> FPMonad
handleKeyInFormula k ms = do
    B.zoom (fpFormula . veEditor) $ Ed.handleEditorEvent (VtyEvent (EvKey k ms))
    use fpIsFormula >>= \case
       False -> return ()
       True -> do
                 f <- use $ fpFormula . veContent
                 t <- use fpType
                 rst <- use fpRst
                 index <- use fpIndex
                 let ex = parseExpression f
                     r = row index rst
                     v = convert t [evaluate r (getDataSources rst) $ addPositions rst ex]
                 fpValue .= Right v
    return DoNothing

returnFieldProperties :: FPMonad
returnFieldProperties = do
    fp <- get
    let name = fp ^. fpName . veContent
        fType = fp ^. fpType
        formulaOrValue = if fp ^. fpIsFormula
                         then Left $ fp ^. fpFormula . veContent
                         else case fp ^. fpValue of
                                 Left ve -> Right $ ve ^. veField
                                 Right f -> Right f
    return . DialogResult $ FieldPropertiesInfo name fType formulaOrValue
updateFieldPropertiesDialog :: Field -> FieldPropertiesDialog -> FieldPropertiesDialog
updateFieldPropertiesDialog f = over fpValue (either (Left . updateEditor f) (Right . const f))

changeFocus :: FieldPropertiesFocus -> FPMonad
changeFocus f = fpFocus .= f >> return DoNothing

onDialog :: (FieldPropertiesDialog -> FieldPropertiesDialog) -> FPMonad
onDialog f = modify f >> return DoNothing

moveUp :: FieldPropertiesDialog -> FieldPropertiesDialog
moveUp = fixFocus True . over fpFocus (\i -> if i > minBound then pred i else minBound)

moveDown :: FieldPropertiesDialog -> FieldPropertiesDialog
moveDown = fixFocus False . over fpFocus (\i -> if i < maxBound then succ i else maxBound)

fixFocus :: Bool -> FieldPropertiesDialog -> FieldPropertiesDialog
fixFocus isUp fp = case fp ^. fpFocus of
                     FpOK -> over fpDialog (setDialogFocus (DButton OkButton)) fp
                     FpCancel -> over fpDialog (setDialogFocus (DButton CancelButton)) fp
                     FpFormula -> if fp ^. fpIsFormula
                                  then fp
                                  else set fpFocus ((if isUp then pred else succ)  FpFormula) fp
                     _ -> fp

nextType :: FieldPropertiesDialog -> FieldPropertiesDialog
nextType fp
   | fp ^. fpType == maxBound = fp
   | otherwise = changeType (succ $ fp ^. fpType) fp

prevType :: FieldPropertiesDialog -> FieldPropertiesDialog
prevType fp
   | fp ^. fpType == minBound = fp
   | otherwise = changeType (pred $ fp ^. fpType) fp

changeType :: FieldType -> FieldPropertiesDialog -> FieldPropertiesDialog
changeType t fp
   | fp ^. fpFocus /= FpType || t == TypeEmpty = fp
   | otherwise = set fpType t $ over fpValue (updateValueViewer f') fp
   where f' = convertKeepText t $ either (toField . view veContent) id (fp ^. fpValue)

