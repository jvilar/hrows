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
import GHC.Generics(Generic)
import GI.Gtk

import Presenter.Input

import GUI.HKD
import Control.Monad.IO.Class (MonadIO)
import Model (RowPos)

data ListingWindow' f = ListingWindow { window :: HKD f Window
                                , listingView :: HKD f TreeView
                                , listingFilterEntry :: HKD f TextView
                                , inputChanLW :: HKD f (Chan Input)
                                } deriving Generic

type ListingWindow = ListingWindow' Identity

getCurrentRow :: MonadIO m => ListingWindow -> m (Maybe RowPos)
getCurrentRow w = do
   path <- fst <$> #getCursor (listingView w)
   case path of
       Nothing -> return Nothing
       Just tp -> #getIndices tp >>= \case
                     Just (r:_) -> return (Just $ fromIntegral r)
                     _ -> return Nothing
