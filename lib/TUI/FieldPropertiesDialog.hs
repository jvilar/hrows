{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.FieldPropertiesDialog (
    FieldPropertiesDialog
  , FieldPropertiesFocus(..)
  , fpFocus
  , fpName
  , fpType
  , fpValue
  , fpIsFormula
  , fpFormula
  , mkFieldPropertiesDialog
  , renderFieldPropertiesDialog
  , updateFieldPropertiesDialog
  , fieldPropertiesMoveUp
  , fieldPropertiesMoveDown
  , fieldPropertiesNextType
  , fieldPropertiesPrevType
  ) where


import Brick ( Widget (Widget), txt, joinBorders, hLimitPercent, withAttr, (<=>), (<+>), getContext, Size (..), vSize, availWidthL, render )
import Brick.Widgets.Border ( borderWithLabel, hBorder )
import Brick.Widgets.Center ( centerLayer, hCenter )
import Brick.Widgets.Dialog ( Dialog, dialog, renderDialog, dialogWidthL, setDialogFocus )
import Control.Lens ( makeLenses, over, (^.), lens, set )
import Data.Maybe(fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Model.Expression ( FieldType(TypeEmpty), ToField(toField), Field, Formula )

import TUI.Base
    ( DialogButton(..),
      ValueEditor,
      HasEditor(..),
      Name(..),
      selectedElementAttr,
      titleAttr,
      ValueViewer,
      updateEditor,
      mkEditor,
      renderValueViewer,
      renderValueEditor )
import Brick.Widgets.Edit (Editor)

data FieldPropertiesFocus = FpName | FpValue | FpType | FpFMark | FpFormula | FpOK | FpCancel deriving (Eq, Ord, Enum, Bounded)

data FieldPropertiesDialog = FieldPropertiesDialog { _fpTitle :: Text
                                     , _fpName :: ValueEditor
                                     , _fpValue :: ValueViewer
                                     , _fpType :: Text
                                     , _fpIsFormula :: Bool
                                     , _fpFormula :: ValueEditor
                                     , _fpFocus :: FieldPropertiesFocus
                                     , _fpDialog :: Dialog () Name
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

mkFieldPropertiesDialog :: Text -> Bool -> Field -> Text -> Maybe Formula -> FieldPropertiesDialog
mkFieldPropertiesDialog fname isFormula field fType mFormula = FieldPropertiesDialog fname
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
                         (txt "Name: " <+> wFocus FpName (renderValueEditor (fp ^. fpName)))
                         <=>
                         hBorder
                         <=>
                         (txt "Value: " <+> renderValueViewer (fp ^. fpValue))
                         <=>
                         hBorder
                         <=>
                         (txt "Type: " <+> wFocus FpType (txt $ fp ^. fpType))
                         <=>
                         hBorder
                         <=>
                         ( (if fp ^. fpIsFormula
                           then wFocus FpFMark (txt "[X]")
                           else wFocus FpFMark (txt "[ ]"))
                           <+>
                           txt " Formula: "
                           <+>
                           renderValueEditor (fp ^. fpFormula)
                         )
                         <=>
                         hBorder
            where wFocus n = if fp ^. fpFocus == n
                      then withAttr selectedElementAttr
                      else id

updateFieldPropertiesDialog :: Field -> FieldPropertiesDialog -> FieldPropertiesDialog
updateFieldPropertiesDialog f = over fpValue (either (Left . updateEditor f) (Right . const f))

fieldPropertiesMoveUp :: FieldPropertiesDialog -> FieldPropertiesDialog
fieldPropertiesMoveUp = fixFocus True . over fpFocus (\i -> if i > minBound then pred i else minBound)

fieldPropertiesMoveDown :: FieldPropertiesDialog -> FieldPropertiesDialog
fieldPropertiesMoveDown = fixFocus False . over fpFocus (\i -> if i < maxBound then succ i else maxBound)

fixFocus :: Bool -> FieldPropertiesDialog -> FieldPropertiesDialog
fixFocus isUp fp = case fp ^. fpFocus of
                     FpOK -> over fpDialog (setDialogFocus (DButton OkButton)) fp
                     FpCancel -> over fpDialog (setDialogFocus (DButton CancelButton)) fp
                     FpFormula -> if fp ^. fpIsFormula
                                  then fp
                                  else set fpFocus ((if isUp then pred else succ)  FpFormula) fp
                     _ -> fp

fieldPropertiesNextType :: FieldPropertiesDialog -> FieldPropertiesDialog
fieldPropertiesNextType fp
   | fp ^. fpFocus /= FpType || t == maxBound || t' == TypeEmpty = fp
   | otherwise = set fpType (T.pack $ show t') fp
   where t = read . T.unpack $ fp ^. fpType
         t' = succ t

fieldPropertiesPrevType :: FieldPropertiesDialog -> FieldPropertiesDialog
fieldPropertiesPrevType fp
   | fp ^. fpFocus /= FpType || t == minBound = fp
   | otherwise = set fpType (T.pack $ show t') fp
   where t :: FieldType
         t = read . T.unpack $ fp ^. fpType
         t' = pred t

