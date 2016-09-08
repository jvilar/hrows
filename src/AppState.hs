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
                , addMessage
                , addMessageM
                , cancelIteration
                , cancelIterationM
                , setModel
                , setModelM
                -- *Reexported modules
                , module Iteration
) where

import Data.Maybe(fromMaybe)

import DisplayInfo
import Iteration
import Model

-- |The state of the application.
data AppState = AppState { model :: Model -- ^The current model.
                         , pos :: Int -- ^The current position.
                         , pendingIteration :: Iteration -- ^The pending iteration with the user.
                         }

-- |Build an state from a model.
mkState :: Model -> AppState
mkState m = AppState { model = m
                     , pos = 0
                     , pendingIteration = NoIteration
                     }

-- |The class representing the types that can update the state of the
-- application.
class StateUpdater t where
    update :: t -> AppState -> IO AppState

instance StateUpdater Message where
    update = addMessageM

instance StateUpdater Model where
    update = setModelM


-- |Builds the information of the display from the current state.
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


-- |Adds a message to be displayed.
addMessage :: Message -> AppState -> AppState
addMessage m s = s { pendingIteration = DisplayMessage m }

-- |Adds a message to be displayed. Monadic version.
addMessageM :: Monad m => Message -> AppState -> m AppState
addMessageM m = return . addMessage m

-- |Cancels the pending iteration.
cancelIteration :: AppState -> AppState
cancelIteration s = s { pendingIteration = NoIteration }

-- |Cancels the pending iteration. Monadic version.
cancelIterationM :: Monad m => AppState -> m AppState
cancelIterationM = return . cancelIteration

-- |Changes the model.
setModel :: Model -> AppState -> AppState
setModel m s = s { model = m }

-- |Changes the model. Monadic version.
setModelM :: Monad m => Model -> AppState -> m AppState
setModelM m = return . setModel m

