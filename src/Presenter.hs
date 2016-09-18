{-# LANGUAGE Arrows, TupleSections #-}

module Presenter (
                 -- *Functions
                 presenter
                 -- *Reexported
                 , module Input
) where

import Control.Auto(Auto, arrM, fromBlips, emitJusts, holdWith_, perBlip, id, (.))
import Prelude hiding((.), id)

import GUICommand
import Input
import Model

-- |The presenter admits inputs and produces information
-- for updating the display.
presenter :: Model -> Auto IO Input [GUICommand]
presenter model0 = proc inp -> do
             rec
               (model, modelList) <- updateModel model0 -< (inp, pos)
               (pos, posList) <- updatePosition 0 -< (inp, model)
               iterList <- updateIteration -< inp
             id -< concat [modelList, posList, iterList]

updateModel :: Model -> Auto IO (Input, Int) (Model, [GUICommand])
updateModel model0 = proc (inp, pos) -> do
                b <- emitJusts getUpdates -< inp
                bmodelCmds <- perBlip (updateAuto model0) -< ((,pos) <$> b)
                model <- holdWith_ model0 -< fst <$> bmodelCmds
                cmds <- fromBlips [] -< snd <$> bmodelCmds
                id -< (model, cmds)

getUpdates :: Input -> Maybe UpdateCommand
getUpdates (InputUpdate cmd) = Just cmd
getUpdates _ = Nothing

updatePosition :: Int -> Auto IO (Input, Model) (Int, [GUICommand])
updatePosition pos0 = proc (inp, model) -> do
                         bmoves <- emitJusts getMoves -< inp
                         bposCmds <- perBlip (movementAuto pos0) -< ((, model) <$> bmoves)
                         pos <- holdWith_ pos0 -< fst <$> bposCmds
                         cmds <- fromBlips [] -< snd <$> bposCmds
                         id -< (pos, cmds)

getMoves :: Input -> Maybe MoveCommand
getMoves (InputMove cmd) = Just cmd
getMoves _ = Nothing

updateIteration :: Auto IO Input [GUICommand]
updateIteration = proc inp -> do
                    b <- emitJusts getDialogs -< inp
                    ds <- perBlip dialogAuto -< b
                    fromBlips [] -< ds

getDialogs :: Input -> Maybe DialogCommand
getDialogs (InputDialog cmd) = Just cmd
getDialogs _ = Nothing
