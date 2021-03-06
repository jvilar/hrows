{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GUI.ListingWindow.Update (
  changeTitle,
  updateNames,
  showFullListing,
  showFieldsRow,
  updatePosition
) where

import Control.Monad(forM_, unless)
import Data.GI.Base.Attributes(clear)
import Data.GI.Base.GType(gtypeString)
import Data.Text (Text)
import GI.Gdk
import GI.Gtk

import GUI.Command
import GUI.ListingWindow
import Data.Int (Int32)
import Model (FieldName, RowPos)


changeTitle :: Text -> ListingWindow -> IO ()
changeTitle title lWindow = set (window lWindow) [ windowTitle := title ]

updateNames :: [FieldName] -> ListingWindow -> IO ()
updateNames names lWindow = do
  let lv = listingView lWindow
  adjustColumns (fromIntegral $ length names) lv
  cols <- #getColumns lv
  forM_ (zip cols names) $ uncurry #setTitle

adjustColumns :: Int32 -> TreeView -> IO ()
adjustColumns ncols tv = do
  current <- fromIntegral <$> #getNColumns tv
  unless (current == ncols) $ clear tv #model
  case compare current ncols of
    LT -> addColumns tv [current .. ncols - 1]
    EQ -> return ()
    GT -> delColumns tv [ncols .. current - 1]

addColumns :: TreeView -> [Int32] -> IO ()
addColumns tv ns =
  forM_ ns $ \n -> do
    col <- treeViewColumnNew
    renderer <- cellRendererTextNew
    #packStart col renderer True
    #addAttribute col renderer "text" n
    #setTitle col "nuevo"
    #appendColumn tv col

delColumns :: TreeView -> [Int32] -> IO ()
delColumns tv ns =
  forM_ (reverse ns) $ \n-> do
    Just col <- #getColumn tv n
    _ <- #removeColumn tv col
    #clear col

showFullListing :: [[Text]] -> ListingWindow -> IO ()
showFullListing [] lw = clear (listingView lw) #model
showFullListing tss lw = do
  let lv = listingView lw
      ncols = length $ head tss
      types = replicate ncols gtypeString
  adjustColumns (fromIntegral ncols) lv
  clear lv #model
  ls <- listStoreNew types
  forM_ tss $ \ts -> do
    iter <- #append ls
    fillRow iter ls $ zip [0..] ts
  #setModel lv $ Just ls

showFieldsRow :: RowPos -> [FieldInfo] -> ListingWindow -> IO ()
showFieldsRow pos fis lw = do
  let lv = listingView lw
  Just model <- #getModel lv
  Just listStore <- castTo ListStore model
  path <- rowPos2Path pos
  iter <- #getIter model path >>= \case
               (True, it) -> return it
               (False, _) -> #append listStore
  fillRow iter listStore [(indexFI fi, textFI fi) | fi <- fis]
  #rowChanged model path iter

fillRow :: TreeIter -> ListStore -> [(Int32, Text)] -> IO ()
fillRow iter listStore = mapM_ $ \(i, t) ->
      toGValue (Just t) >>= #setValue listStore iter i

rowPos2Path :: RowPos -> IO TreePath
rowPos2Path pos = do
  path <- treePathNew
  #appendIndex path $ fromIntegral pos
  return path

updatePosition :: RowPos -> ListingWindow -> IO ()
updatePosition pos lw = do
  cRow <- getCurrentRow lw
  unless (cRow == Just pos) $ do
    let lv = listingView lw
    path <- rowPos2Path pos
    #setCursor lv path (Nothing :: Maybe TreeViewColumn) False
