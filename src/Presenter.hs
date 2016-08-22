{-# LANGUAGE Arrows #-}

module Presenter (
                 -- *Types
                 Input(..)
                 , MoveCommand
                 -- *Functions
                 , presenter
) where

import Control.Auto(Auto', accum_, id, (.))
import Prelude hiding((.), id)

import Model

-- |The input that the presenter can receive.
data Input = InputMove MoveCommand

-- |Commands related to movement.
data MoveCommand = MoveNext
                 | MovePrevious

-- |The information to display
data DisplayInfo = DisplayInfo { rows :: [String]
                               , position :: Int
                               }

data State = State { model :: Model
                   , pos :: Int
                   }

state0 :: State
state0 = State { model = model0
               , pos = 0
               }

presenter :: Auto' Input DisplayInfo
presenter = proc inp -> do
              state <- accum_ update state0 -< inp
              id -< buildDisplay state

update :: State -> Input -> State
update = undefined

buildDisplay :: State -> DisplayInfo
buildDisplay = undefined
