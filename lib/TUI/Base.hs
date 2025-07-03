{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , veType
    , veField
    , veContent
    , updateValueViewer
    , vvValue
    , updateEditor
    , mkEditor
    , renderName
    , renderValue
    , renderValueViewer
    , renderValueEditor
    , myTxt
    , valueList
    , maxWidth
) where


import Brick ( AttrName, AttrMap, attrName, attrMap, Widget (..), Size (Fixed), Context (availWidth), getContext, txt, withAttr )
import Brick.Widgets.Dialog (buttonSelectedAttr)
import Brick.Widgets.Edit(Editor, getEditContents, applyEdit, editor, renderEditor)
import Control.Lens (Lens', makeLenses, (^.), set, over, lens, Getter, to)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as Tz
import Data.Vector qualified as V
import Graphics.Vty.Attributes (defAttr, bold, reverseVideo, withStyle, withBackColor, withForeColor, black, rgbColor)

import Model.Field ( Field, isError, toString, FieldType, typeOf )
import Model.RowStore (RowStore, isFormula)
import Brick.Widgets.List (List, list)
import Model (row)


maxWidth :: Int
maxWidth = 40

data Name = DButton DialogButton
          | FieldNames
          | SearchList
          | FieldPropertiesNameEditor
          | FieldPropertiesValueEditor
          | FieldPropertiesFormulaEditor
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
                            , (errorAttr, withBackColor (withForeColor defAttr black) $ rgbColor 255 (160 :: Int) 0)
                            ]

data ValueEditor = ValueEditor { _veEditor :: Editor Text Name
                               , _veIsError :: Bool
                               , _veType :: FieldType
                               , _veField :: Field
                               }

makeLenses ''ValueEditor

type ValueViewer = Either ValueEditor Field

updateValueViewer :: Field -> ValueViewer -> ValueViewer
updateValueViewer f = either (Left . updateEditor f) (const $ Right f)

vvValue :: Lens' ValueViewer Field
vvValue = lens getter setter
    where getter (Left ve) = ve ^. veField
          getter (Right f) = f
          setter (Left ve) f = Left $ set veField f ve
          setter (Right _) f = Right f

updateEditor :: Field -> ValueEditor -> ValueEditor
updateEditor f ve = over veEditor (applyEdit (const $ Tz.textZipper [toString f] $ Just 1))
                         $ set veIsError (isError f)
                         $ set veType (typeOf f)
                         $ set veField f ve

veContent :: Getter ValueEditor Text
veContent = to $ T.concat . getEditContents . _veEditor

mkEditor :: Name -> Field -> ValueEditor
mkEditor n f = ValueEditor (editor n (Just 1) $ toString f) (isError f) (typeOf f) f

renderValue :: Bool -> Text -> Widget Name
renderValue = renderElement

renderElement :: Bool -> Text -> Widget Name
renderElement False = myTxt
renderElement True = withAttr selectedElementAttr . myTxt

renderValueViewer :: ValueViewer -> Widget Name
renderValueViewer = either renderValueEditor renderField
  where renderField f = let
              attr = if isError f then errorAttr else formulaAttr
           in withAttr attr . myTxt $ toString f

renderValueEditor :: ValueEditor -> Widget Name
renderValueEditor ve = renderEditor ((if ve ^. veIsError
                        then withAttr errorAttr
                        else id) . (txt . T.unlines)) True $ ve ^. veEditor

renderName :: Bool -> Text -> Widget Name
renderName = (withAttr titleAttr .) . renderElement

myTxt :: Text -> Widget n
myTxt t = Widget Fixed Fixed $ do
      w <- availWidth <$> getContext
      let l = T.length t
          t' | l <= w = T.justifyLeft w ' ' t
             | otherwise = T.take (w-3) t <> "..."
      render $ txt t'

valueList :: Int -> RowStore -> List Name ValueViewer
valueList pos rst = list ValueList (V.fromList $ zipWith valueWidget [0..] (row pos rst)) 1
    where valueWidget n f
              | isFormula n rst = Right f
              | otherwise = Left $ mkEditor (ValueViewer $ fromIntegral n) f
