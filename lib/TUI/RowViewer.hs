{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.RowViewer (
  RowViewer
  , rvFieldNames
  , rvValueList
  , mkRowViewer
  , renderRowViewer
  , updateRvNames
  , updateRvValues
  ) where


import Brick ( Widget(Widget, render), availHeight, Size(..), vLimit, getContext, hBox, hLimit )
import Brick.Widgets.Border ( vBorder )
import Brick.Widgets.List ( List, listElements, listSelected, listReplace, listSelectedElement, list, renderList, listMoveTo )
import Control.Lens ( makeLenses, over, (^.), lens, set, element )
import Data.Maybe(fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Model.Field (Field)
import Model.RowStore.Base (RowStore, fnames)

import TUI.Base
    ( HasEditor(..),
      Name(FieldNames),
      ValueViewer,
      updateValueViewer,
      renderValueViewer,
      renderName,
      valueList )


data RowViewer = RowViewer { _rvFieldNames :: List Name Text
                           , _rvFieldWidth :: Int
                           , _rvValueList :: List Name ValueViewer
                           }

makeLenses ''RowViewer

instance HasEditor RowViewer where
    editorLens = lens getter setter
        where getter rv = do
                             (_, Left ve) <- listSelectedElement $ rv ^. rvValueList
                             return ve
              setter _ Nothing = error "Cannot remove editor from row viewer"
              setter rv (Just ve) = set (rvValueList . element (fromMaybe 0 $ listSelected $ rv ^. rvValueList)) (Left ve) rv


updateRvValues :: [Field] -> RowViewer -> RowViewer
updateRvValues ts rv = over rvValueList (listReplace v i) rv
  where i = listSelected $ rv ^. rvValueList
        l = listElements (rv ^. rvValueList)
        v = V.fromList $ zipWith updateValueViewer ts (V.toList l)

updateRvNames :: [Text] -> RowViewer -> RowViewer
updateRvNames ns = set rvFieldNames (list FieldNames (V.fromList ns) 1)

maxWidth :: Int
maxWidth = 40

mkRowViewer :: RowStore -> Int -> RowViewer
mkRowViewer rst pos = RowViewer { _rvFieldNames = listMoveTo pos fl
                                , _rvFieldWidth = min maxWidth (V.maximum . V.map T.length $ listElements fl)
                                , _rvValueList = valueList pos rst
                                }
                            where fl = fieldList rst

fieldList :: RowStore -> List Name Text
fieldList rst = list FieldNames (V.fromList $ fnames rst) 1


renderRowViewer :: RowViewer -> Widget Name
renderRowViewer rv = Widget Greedy Fixed $ do
    h <- availHeight <$> getContext
    let v = min (V.length $ listElements $ rv ^. rvFieldNames) (h-2)
    render $ vLimit v $ hBox [
                hLimit (rv ^. rvFieldWidth) (renderList renderName False $ rv ^. rvFieldNames)
                , vBorder
                , renderList (const renderValueViewer) False
                      $ rv ^. rvValueList
              ]

