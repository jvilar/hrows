{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}

module GUI.ListingWindow (
  -- *Types
  ListingWindow
  , ListingWindow'(..)
  -- *Functions
  , getCurrentRow
  ) where

import Control.Concurrent.Chan(Chan)
import Data.IntMap (IntMap)
import GHC.Generics(Generic)
import GI.Gtk

import Presenter.Input

import GUI.HKD
import Control.Monad.IO.Class (MonadIO)
import Model (RowPos)

-- |A ListingWindow is a window that shows a listing that it stored in a `TreeView`.
data ListingWindow' f = ListingWindow { windowLW :: HKD f Window -- ^The window.
                                      , viewLW :: HKD f TreeView -- ^The `TreeView` that stores the listing.
                                      , filterEntryLW :: HKD f TextView -- ^The entry for the filter.
                                      , filteredPosLW :: HKD f (IntMap RowPos) -- ^A map that associates the original row to 
                                                                               -- the row after the filter is applied.
                                      , rendererLW :: HKD f CellRendererText -- ^The renderer used for the view.
                                      , inputChanLW :: HKD f (Chan Input) -- ^The input chan.
                                      } deriving Generic

type ListingWindow = ListingWindow' Identity

getCurrentRow :: MonadIO m => ListingWindow -> m (Maybe RowPos)
getCurrentRow w = do
   path <- fst <$> #getCursor (viewLW w)
   case path of
       Nothing -> return Nothing
       Just tp -> #getIndices tp >>= \case
                     Just (r:_) -> return (Just $ fromIntegral r)
                     _ -> return Nothing
