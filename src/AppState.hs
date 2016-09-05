{-# LANGUAGE FlexibleInstances, UndecidableInstances  #-}

module AppState (
                 -- *Types
                 AppState(..)
                -- *Classes
                , StateUpdater(..)
                -- *Constructor
                , mkState
                -- *Functions
                , buildDisplay
                -- *Reexported modules
                , module Iteration
) where

import Data.Maybe(fromMaybe)

import DisplayInfo
import Iteration
import Model

data AppState = AppState { model :: Model
                         , pos :: Int
                         , pendingIteration :: Iteration
                         }

mkState :: Model -> AppState
mkState m = AppState { model = m
                     , pos = 0
                     , pendingIteration = NoIteration
                     }

class StateUpdater t where
    update :: t -> AppState -> IO AppState

buildDisplay :: AppState -> DisplayInfo
buildDisplay s = DisplayInfo { position = p
                             , fields = map toString r
                             , fieldNames = fnames
                             , modelSize = size m
                             , iteration = pendingIteration s
                             }
                   where p = pos s
                         m = model s
                         r = row p m
                         fnames = map (++ ": ") $ fromMaybe
                                  (take (length r) $ map (("Campo " ++).show) [1 :: Int ..])
                                  (names m)
