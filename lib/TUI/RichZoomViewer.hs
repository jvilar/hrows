{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.RichZoomViewer (
    RichZoomViewer
  , RichZoomFocus(..)
  , ivFocus
  , ivValue
  , ivIsFormula
  , mkRichZoomViewer
  , renderRichZoomViewer
  , updateRichZoomViewer
  , richZoomMoveUp
  , richZoomMoveDown
  , richZoomNextType
  , richZoomPrevType
  ) where


import Brick ( Widget, txt, joinBorders, hLimitPercent, withAttr, (<=>), (<+>) )
import Brick.Widgets.Border ( borderWithLabel, hBorder )
import Brick.Widgets.Center ( centerLayer, hCenter )
import Control.Lens ( makeLenses, over, (^.), lens, set )
import Data.Maybe(fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Model.Expression ( FieldType(TypeEmpty), ToField(toField), Field, Formula )

import TUI.Base
    ( ValueEditor,
      HasEditor(..),
      Name(RichZoomFormulaEditor, RichZoomValueEditor),
      selectedElementAttr,
      titleAttr,
      ValueViewer,
      updateEditor,
      mkEditor,
      renderValueViewer,
      renderValueEditor )

data RichZoomFocus = RzValue | RzType | RzFMark | RzFormula deriving (Eq, Ord, Enum)

data RichZoomViewer = RichZoomViewer { _ivTitle :: Text
                                     , _ivValue :: ValueViewer
                                     , _ivType :: Text
                                     , _ivIsFormula :: Bool
                                     , _ivFormula :: ValueEditor
                                     , _ivFocus :: RichZoomFocus
                                     }

makeLenses ''RichZoomViewer

instance HasEditor RichZoomViewer where
    editorLens = lens getter setter
        where getter iv = case iv ^. ivFocus of
                              RzValue -> case iv ^. ivValue of
                                             Left ve -> Just ve
                                             _ -> Nothing
                              RzFormula -> if iv ^. ivIsFormula
                                             then Just $ iv ^. ivFormula
                                             else Nothing
                              _ -> Nothing
              setter _ Nothing = error "Cannot remove editor from rich zoom viewer"
              setter iv (Just ve) = case iv ^. ivFocus of
                                      RzValue -> set ivValue (Left ve) iv
                                      RzFormula -> set ivFormula ve iv
                                      _ -> error "Bad focus in rich zoom for setting editor"

mkRichZoomViewer :: Text -> Bool -> Field -> Text -> Maybe Formula -> RichZoomViewer
mkRichZoomViewer fname isFormula field fType mFormula = RichZoomViewer fname
                            (if isFormula
                             then Right field
                             else Left (mkEditor RichZoomValueEditor field)
                             )
                            fType
                            (isJust mFormula)
                            (mkEditor RichZoomFormulaEditor $ toField $ fromMaybe "" mFormula)
                            RzValue

renderRichZoomViewer :: RichZoomViewer -> Widget Name
renderRichZoomViewer iv = centerLayer $ joinBorders $
                       hLimitPercent 95 $
                       borderWithLabel (withAttr titleAttr $ txt $ iv ^. ivTitle) $
                         renderValueViewer (iv ^. ivValue)
                         <=>
                         hBorder
                         <=>
                          wFocus RzType (txt $ iv ^. ivType)
                         <=>
                         hBorder
                         <=>
                         if iv ^. ivIsFormula
                         then wFocus RzFMark (txt "[X]") <+> txt " " <+> renderValueEditor (iv ^. ivFormula)
                         else wFocus RzFMark (txt "[ ]")
                         <=>
                         hBorder
                         <=>
                         hCenter (txt "C-r: close rich zoom, C-z: zoom")
            where wFocus n = if iv ^. ivFocus == n
                      then withAttr selectedElementAttr
                      else id

updateRichZoomViewer :: Field -> RichZoomViewer -> RichZoomViewer
updateRichZoomViewer f = over ivValue (either (Left . updateEditor f) (Right . const f))

richZoomMoveUp :: RichZoomViewer -> RichZoomViewer
richZoomMoveUp = over ivFocus (\i -> if i > RzValue then pred i else RzValue)

richZoomMoveDown :: RichZoomViewer -> RichZoomViewer
richZoomMoveDown rz = over ivFocus (\i -> if i < limit then succ i else limit) rz
  where
    limit = if rz ^. ivIsFormula then RzFormula else RzFMark

richZoomNextType :: RichZoomViewer -> Maybe (RichZoomViewer, FieldType)
richZoomNextType rz
   | rz ^. ivFocus /= RzType || t == maxBound || t' == TypeEmpty = Nothing
   | otherwise = Just (set ivType (T.pack $ show t') rz, t')
   where t = read . T.unpack $ rz ^. ivType
         t' = succ t

richZoomPrevType :: RichZoomViewer -> Maybe (RichZoomViewer, FieldType)
richZoomPrevType rz
   | rz ^. ivFocus /= RzType || t == minBound = Nothing
   | otherwise = Just (set ivType (T.pack $ show t') rz, t')
   where t = read . T.unpack $ rz ^. ivType
         t' = pred t

