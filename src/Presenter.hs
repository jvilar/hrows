{-# LANGUAGE Arrows #-}

module Presenter (
                 -- *Functions
                 presenter
                 -- *Reexported
                 , module Input
) where

import Control.Auto(Auto', accum_, id)
import Prelude hiding((.), id)

import AppState
import DisplayInfo
import Input

-- |The presenter admits inputs and produces information
-- for updating the display.
presenter :: AppState -> Auto' Input DisplayInfo
presenter state0 = proc inp -> do
              state <- accum_ (flip update) state0 -< inp
              id -< buildDisplay state
