{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GUI.ListingWindow.Update (
  changeTitle,
  updateNames,
  showFullListing,
  showFieldsRow,
  updatePosition,
  showFilterStatus
) where

import Control.Monad(forM_, unless, zipWithM_)
import Data.GI.Base.Attributes(clear)
import Data.GI.Base.GType(gtypeString)
import Data.Text (Text)
import GI.Gdk
import GI.Gtk

import GUI.Command
import GUI.ListingWindow
import Data.Int (Int32)
import Model (FieldName, RowPos)

-- |Change the tile of the `ListingWindow`.
changeTitle :: Text -> ListingWindow -> IO ()
changeTitle title lWindow = set (windowLW lWindow) [ windowTitle := title ]

-- |Change the titles of the columns in the `ListingWindow`.
updateNames :: [FieldName] -> ListingWindow -> IO ()
updateNames names lWindow = do
  let tv = viewLW lWindow
  adjustColumns (fromIntegral $ length names) (rendererLW lWindow) tv
  cols <- #getColumns tv
  zipWithM_ #setTitle cols names

-- |Add or delete columns necessary to make that the `TreeView` has
-- the given number of columns.
adjustColumns :: Int32 -> CellRendererText -> TreeView -> IO ()
adjustColumns ncols renderer tv = do
  current <- fromIntegral <$> #getNColumns tv
  case compare current ncols of
    LT -> addColumns tv renderer [current .. ncols - 1]
    EQ -> return ()
    GT -> delColumns tv [ncols .. current - 1]

-- |Add columns in the positions given by the list. The columns
-- will have a default name.
addColumns :: TreeView -> CellRendererText -> [Int32] -> IO ()
addColumns tv renderer ns =
  forM_ ns $ \n -> do
    col <- treeViewColumnNew
    #packStart col renderer True
    #addAttribute col renderer "text" n
    #setTitle col "nuevo"
    #appendColumn tv col

-- |Delete the columns in the positions given by the list. 
-- The list is assumed to be ordered.
delColumns :: TreeView -> [Int32] -> IO ()
delColumns tv ns =
  forM_ (reverse ns) $ \n-> do
    Just col <- #getColumn tv n
    _ <- #removeColumn tv col
    #clear col

showFullListing :: [[Text]] -> ListingWindow -> IO ()
showFullListing [] lw = clear (viewLW lw) #model
showFullListing tss lw = do
  let tv = viewLW lw
      ncols = length $ head tss
      types = replicate ncols gtypeString
  adjustColumns (fromIntegral ncols) (rendererLW lw) tv
  clear tv #model
  ls <- listStoreNew types
  forM_ tss $ \ts -> do
    iter <- #append ls
    fillRow iter ls $ zip [0..] ts
  #setModel tv $ Just ls

showFieldsRow :: RowPos -> [FieldInfo] -> ListingWindow -> IO ()
showFieldsRow pos fis lw = do
  let tv = viewLW lw
  Just model <- #getModel tv
  Just listStore <- castTo ListStore model
  path <- rowPos2Path pos
  iter <- #getIter model path >>= \case
               (True, it) -> return it
               (False, _) -> #append listStore
  fillRow iter listStore [(indexFI fi, textFI fi) | fi <- fis, isVisibleFI fi]
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
    let tv = viewLW lw
    path <- rowPos2Path pos
    #setCursor tv path (Nothing :: Maybe TreeViewColumn) False

showFilterStatus:: Bool -> ListingWindow -> IO ()
showFilterStatus isOK lw = #setName (filterEntryLW lw) $ if isOK
                                                         then "normal"
                                                         else "error"
