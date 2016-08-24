module AppState (
                 -- *Types
                 AppState(..)
                -- *Classes
                , StateUpdater(..)
                -- *Constants
                , state0
                -- *Functions
                , buildDisplay
) where

import Data.Maybe(fromMaybe)

import DisplayInfo
import Model

data AppState = AppState { model :: Model
                         , pos :: Int
                         }

state0 :: AppState
state0 = AppState { model = model0
                  , pos = 0
                  }

class StateUpdater t where
    update :: t -> AppState -> AppState

buildDisplay :: AppState -> DisplayInfo
buildDisplay s = DisplayInfo { position = p
                             , fields = map toString r
                             , fieldNames = fnames
                             , modelSize = size m
                             }
                   where p = pos s
                         m = model s
                         r = row p m
                         fnames = map (++ ": ") $ fromMaybe
                                  (take (length r) $ map (("Campo " ++).show) [1 :: Int ..])
                                  (names m)
