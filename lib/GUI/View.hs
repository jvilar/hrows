{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GUI.View 
  (
  View(..)
  )
where

import Data.Text (Text)
import GI.Gtk

import qualified GUI.ListingWindow as LW
import qualified GUI.ListingWindow.Update as LW
import qualified GUI.MainWindow as MW
import qualified GUI.MainWindow.Update as MW
import Model (FieldName, RowPos)


class View v where
  window :: v -> Window
  changeTitle :: Text -> v -> IO ()
  updateNames :: [FieldName] -> v -> IO ()
  updatePosition :: RowPos -> Int -> v -> IO ()

instance View LW.ListingWindow where
  window = LW.window
  changeTitle = LW.changeTitle
  updateNames = LW.updateNames
  updatePosition p _ = LW.updatePosition (p - 1)

instance View MW.MainWindow where
  window = MW.window
  changeTitle = MW.changeTitle
  updateNames = MW.updateNames
  updatePosition = MW.updatePosition
