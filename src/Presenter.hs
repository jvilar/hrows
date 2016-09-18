{-# LANGUAGE Arrows, TupleSections #-}

module Presenter (
                 -- *Functions
                 presenter
                 -- *Reexported
                 , module Input
) where

import Control.Auto(Auto, arrM, emitJusts, holdWith_, perBlip, id, (.))
import Prelude hiding((.), id)

import AppState
import DisplayInfo
import Input
import Model

-- |The presenter admits inputs and produces information
-- for updating the display.
presenter :: AppState -> Auto IO Input DisplayInfo
presenter state0 = proc inp -> do
             rec
               m <- updateModel (model state0) -< (inp, p)
               p <- updatePosition (pos state0) -< (inp, m)
               pending <- updatePending (pendingIteration state0) -< inp
             id -< buildDisplay $ AppState m p pending

updateModel :: Model -> Auto IO (Input, Int) Model
updateModel model0 = proc (inp, pos) -> do
                b <- emitJusts getUpdates -< inp
                u <- perBlip (updateAuto model0) -< ((,pos) <$> b)
                model <- holdWith_ model0 -< u
                id -< model

getUpdates :: Input -> Maybe UpdateCommand
getUpdates (InputUpdate cmd) = Just cmd
getUpdates _ = Nothing

updatePosition :: Int -> Auto IO (Input, Model) Int
updatePosition pos0 = proc (inp, model) -> do
                         b <- emitJusts getMoves -< inp
                         u <- perBlip (movementAuto pos0) -< ((, model) <$> b)
                         pos <- holdWith_ pos0 -< u
                         id -< pos

getMoves :: Input -> Maybe MoveCommand
getMoves (InputMove cmd) = Just cmd
getMoves _ = Nothing

updatePending :: Iteration -> Auto IO Input Iteration
updatePending = arrM . const . return
