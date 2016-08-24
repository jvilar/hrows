{-# LANGUAGE Arrows #-}

module Presenter (
                 -- *Functions
                 presenter
                 -- *Constants
                 , display0
                 -- *Reexported
                 , module Input
) where

import Control.Auto(Auto', accum_, id)
import Prelude hiding((.), id)

import AppState
import DisplayInfo
import Input


-- |Initial display of the application.
display0 :: DisplayInfo
display0 = buildDisplay state0

-- |The presenter admits inputs and produces information
-- for updating the display.
presenter :: Auto' Input DisplayInfo
presenter = proc inp -> do
              state <- accum_ (flip update) state0 -< inp
              id -< buildDisplay state
