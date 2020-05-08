{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI.ListingWindow.Update (
  changeTitle,
  updateNames
) where

import Control.Monad(forM_, unless)
import Data.GI.Base.Attributes(clear)
import Data.GI.Base.GType(gtypeInt, gtypeString)
import Data.Text (Text)
import GI.Gdk
import GI.Gtk

import GUI.ListingWindow
import Data.Int (Int32)
import Model (FieldName)


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
    #addAttribute col renderer "text" n
    #packEnd col renderer True
    #setTitle col "nuevo"
    #appendColumn tv col

delColumns :: TreeView -> [Int32] -> IO ()
delColumns tv ns =
  forM_ ns $ \n-> do
    Just col <- #getColumn tv n
    #removeColumn tv col
    #clear col

-- TODO: delete
fillTreeView :: ListingWindow -> IO ()
fillTreeView w = do
  ls <- listStoreNew [gtypeInt, gtypeString]
  forM_ [(1, "uno"), (2, "dos"), (3, "tres")] $ \(n, str) -> do
      iter <- #append ls
      v <- toGValue (n :: Int32)
      #setValue ls iter 0 v
      v <- toGValue (Just str :: Maybe Text)
      #setValue ls iter 1 v
  let tv = listingView w

  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew

  #packStart col1 renderer1 True
  #packStart col2 renderer2 True

  #addAttribute col1 renderer1 "text" 0
  #addAttribute col2 renderer2 "text" 1

  #setTitle col1 "Número"
  #setTitle col2 "Letra"

  #appendColumn tv col1
  #appendColumn tv col2

  #setModel tv $ Just ls