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
) where

import Data.Maybe(fromMaybe)

import DisplayInfo
import Message
import Model

data AppState = AppState { model :: Model
                         , pos :: Int
                         , pendingMessage :: Maybe Message
                         }

mkState :: Model -> AppState
mkState m = AppState { model = m
                     , pos = 0
                     , pendingMessage = Nothing
                     }

class StateUpdater t where
    update :: t -> AppState -> IO AppState

buildDisplay :: AppState -> DisplayInfo
buildDisplay s = DisplayInfo { position = p
                             , fields = map toString r
                             , fieldNames = fnames
                             , modelSize = size m
                             , message = pendingMessage s
                             }
                   where p = pos s
                         m = model s
                         r = row p m
                         fnames = map (++ ": ") $ fromMaybe
                                  (take (length r) $ map (("Campo " ++).show) [1 :: Int ..])
                                  (names m)
