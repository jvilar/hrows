{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.ZoomViewer (
    ZoomViewer
  , zvTitle
  , zvValue
  , mkZoomViewer
  , renderZoomViewer
  , updateZoomViewer
  ) where


import Brick ( Widget, txt, joinBorders, hLimitPercent, withAttr, (<=>) )
import Brick.Widgets.Border ( borderWithLabel, hBorder )
import Brick.Widgets.Center ( centerLayer, hCenter )
import Control.Lens ( makeLenses, over, (^.), lens, set )
import Data.Text (Text)

import Model.RowStore ( Field )
import TUI.Base
    ( HasEditor(..),
      Name(ZoomEditor),
      titleAttr,
      ValueViewer,
      mkEditor,
      updateEditor,
      renderValueViewer )


data ZoomViewer = ZoomViewer { _zvTitle :: Text
                             , _zvValue :: ValueViewer
                             }

makeLenses ''ZoomViewer

instance HasEditor ZoomViewer where
    editorLens = lens getter setter
        where getter zv = case zv ^. zvValue of
                             Left ve -> Just ve
                             _ -> Nothing
              setter _ Nothing = error "Cannot remove editor from zoom viewer"
              setter zv (Just ve) = set zvValue (Left ve) zv

renderZoomViewer :: ZoomViewer -> Widget Name
renderZoomViewer zv = centerLayer $ joinBorders $
                       hLimitPercent 95 $
                       borderWithLabel (withAttr titleAttr $ txt $ zv ^. zvTitle) $
                         renderValueViewer (zv ^. zvValue)
                         <=>
                         hBorder
                         <=>
                         hCenter (txt "C-r: rich zoom, C-z: close zoom")

mkZoomViewer :: Text -> Bool -> Field -> ZoomViewer
mkZoomViewer fname isFormula field = ZoomViewer fname $ if isFormula
                                                        then Right field
                                                        else Left (mkEditor ZoomEditor field)

updateZoomViewer :: Field -> ZoomViewer -> ZoomViewer
updateZoomViewer f = over zvValue (either (Left . updateEditor f) (Right . const f))

