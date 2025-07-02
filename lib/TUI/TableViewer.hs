{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI.TableViewer (
  TableViewer
  , tvLists
  , tvFieldNames
  , tvCurrentField
  , buildTable
  , renderTableViewer
  ) where


import Brick (Widget(Widget, render),
      Context(availWidth, availHeight),
      Size(Fixed, Greedy),
      Padding(Max),
      vLimit,
      withAttr,
      (<=>),
      getContext,
      hBox,
      hLimit,
      padRight)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.List (List, GenericList(listElements), list, renderList)
import Control.Lens
    ( Traversal',
      makeLenses,
      over,
      (^.),
      lens,
      taking,
      traversed,
      dropping )
import Data.List(transpose, intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Model.Expression (toString)
import Model.Field ( toString )
import Model.RowStore (RowStore, fnames, rows)

import TUI.Base (HasEditor(..), Name(ValueColumn), selectedElementAttr, titleAttr, renderValue, myTxt)

data TableViewer = TableViewer { _tvFieldNames :: [Text]
                               , _tvColWidths :: [Int]
                               , _tvColumns :: [List Name Text]
                               , _tvCurrentField :: Int
                               }

tvLists :: Traversal' TableViewer (List Name Text)
tvLists f (TableViewer fl cw cs cf) = TableViewer fl cw <$> traverse f cs <*> pure cf

makeLenses ''TableViewer

instance HasEditor TableViewer where
    editorLens = lens (const Nothing) setter
        where setter _ Nothing = error "Cannot remove editor from table viewer"
              setter _ _ = error "Cannot set editor in table viewer"

buildTable :: RowStore -> Int -> TableViewer
buildTable rst = TableViewer ns ws cls
    where cs = transpose $ map (map toString) $ rows rst
          cls = zipWith f [0..] cs
          f n r = list (ValueColumn n) (V.fromList r) 1
          ns = fnames rst
          ws = zipWith max (map T.length ns)
                           $ map (maximum . map T.length) cs

renderTableViewer :: TableViewer -> Widget Name
renderTableViewer tv = Widget Greedy Fixed $ do
    h <- availHeight <$> getContext
    w <- availWidth <$> getContext
    let v = min (V.length $ listElements $ head $ tv ^. tvColumns) (h-4)
        ws = allocateWidths (tv ^. tvCurrentField) w $ tv ^. tvColWidths
    render ( vLimit 1 (hBox $ withWidths (\(i, t) ->
                                              if i == tv ^. tvCurrentField
                                              then withAttr selectedElementAttr $ myTxt t
                                              else withAttr titleAttr $ myTxt t)
                               ws
                               (zip [0..] $ tv ^. tvFieldNames)
                      )
             <=>
             hBorder
             <=>
             vLimit v ( hBox $ withWidths (renderList renderValue False)
                               ws
                               (tv ^. tvColumns)
                      )
           )


allocateWidths :: Int -> Int -> [Int] -> [Int]
allocateWidths curF w ws
  | w < s = let
               (lft, wcurF:rgt) = splitAt curF ws1
               wCurrent = min w (wcurF - 1)
               wsLeft= reverse . upto (w - wCurrent) $ reverse lft
               wRest = w - wCurrent - sum wsLeft
            in map (max 0 . subtract 1) $ wsLeft ++ [wCurrent + 1] ++ upto wRest rgt
  | otherwise = over (taking n traversed) (+ (dw+1))
              $ over (dropping n traversed) (+ dw) ws
  where ws1 = map succ ws
        s = sum ws1
        l = length ws1
        (dw, n) = (w - s) `divMod` l
        upto _ [] = []
        upto ml (x:xs) = v:upto (ml-v) xs
            where v = min ml x

withWidths :: (a -> Widget Name) -> [Int] -> [a] -> [Widget Name]
withWidths f ws l = intersperse vBorder
                  $ map (uncurry $ withWidth f)
                  $ filter ((>0) . fst)
                  $ zip ws l

withWidth :: (a -> Widget Name) -> Int -> a -> Widget Name
withWidth f w = hLimit w . padRight Max . f
