{-# LANGUAGE TemplateHaskell #-}

module TUI.Base (
    Name(..)
    , DialogButton(..)
    , HasEditor(..)
    , selectedElementAttr
    , titleAttr
    , formulaAttr
    , errorAttr
    , myAttrMap
    , ValueViewer
    , ValueEditor(..)
    , veEditor
    , veIsError
) where


import Brick ( AttrName, AttrMap, attrName, attrMap )
import Brick.Widgets.Dialog (buttonSelectedAttr)
import Brick.Widgets.Edit(Editor)
import Control.Lens (Lens', makeLenses)
import Data.Text (Text)
import Graphics.Vty.Attributes (defAttr, bold, reverseVideo, withStyle, withBackColor, withForeColor, black, rgbColor)

import Model.Field ( Field )


data Name = DButton DialogButton
          | FieldNames
          | SearchList
          | RichZoomValueEditor
          | RichZoomFormulaEditor
          | ValueColumn Int
          | ValueViewer Int
          | ValueList
          | ZoomEditor
          deriving (Eq, Ord, Show)

data DialogButton = OkButton | CancelButton deriving (Eq, Ord, Show)

class HasEditor i where
    editorLens :: Lens' i (Maybe ValueEditor)

selectedElementAttr :: AttrName
selectedElementAttr = attrName "selectedElement"

titleAttr :: AttrName
titleAttr = attrName "title"

formulaAttr :: AttrName
formulaAttr = attrName "formula"

errorAttr :: AttrName
errorAttr = attrName "error"

myAttrMap :: AttrMap
myAttrMap = attrMap defAttr [ (selectedElementAttr, withStyle defAttr reverseVideo)
                            , (buttonSelectedAttr, withStyle defAttr reverseVideo)
                            , (titleAttr, withStyle defAttr bold)
                            , (formulaAttr, withBackColor (withForeColor defAttr black) $ rgbColor (160 :: Int) 255 255)
                            , (errorAttr, withBackColor (withForeColor defAttr black) $ rgbColor 255 (160 :: Int) 255)
                            ]

data ValueEditor = ValueEditor { _veEditor :: Editor Text Name
                               , _veIsError :: Bool
                               }

makeLenses ''ValueEditor

type ValueViewer = Either ValueEditor Field

