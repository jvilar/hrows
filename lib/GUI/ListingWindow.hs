{-# LANGUAGE DeriveGeneric #-}

module GUI.ListingWindow (
  -- *Types
  ListingWindow
  , ListingWindow'(..)
  ) where

import Control.Concurrent.Chan(Chan)
import GHC.Generics(Generic)
import GI.Gtk

import Presenter.Input

import GUI.HKD
import Data.IORef (IORef)

data ListingWindow' f = ListingWindow { window :: HKD f Window
                                , listingView :: HKD f TreeView
                                , listingFilterEntry :: HKD f TextView
                                , inputChanLW :: HKD f (Chan Input)
                                } deriving Generic

type ListingWindow = ListingWindow' Identity
