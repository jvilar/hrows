{-# LANGUAGE Arrows, TupleSections #-}

module Presenter (
                 -- *Functions
                 presenter
                 -- *Reexported
                 , module Input
) where

import Control.Arrow(arr, first, (>>>))
import Control.Auto(Auto, accum_, accumM_, arrM, delay_, fromBlips, emitJusts, holdWith_, perBlip, stepAuto, id, (.))
import Control.Monad(foldM)
import Data.Maybe(fromMaybe, isJust)
import Data.Monoid((<>))
import Prelude hiding((.), id)

import GUI.Command
import Input
import Model


presenter :: Model -> Auto IO Input [GUICommand]
presenter model0 = arr (:[]) >>> updater model0

updater :: Model -> Auto IO [Input] [GUICommand]
updater model0 = proc inputs -> do
    rec
        dauto <- delay_ (processInput model0) -< auto
        (cmds, auto) <- arrM (uncurry pr) -< (dauto, inputs)
    id -< cmds

pr :: Auto IO Input ([GUICommand], [Input]) -> [Input] -> IO ([GUICommand], Auto IO Input ([GUICommand], [Input]))
pr auto [] = return ([], auto)
pr auto (i:is) = do
    ((cmds, is'), auto') <- stepAuto auto i
    (cmds', auto'') <- pr auto' (is ++ is')
    return (cmds ++ cmds', auto'')

-- |The presenter admits inputs and produces information
-- for updating the display.
processInput :: Model -> Auto IO Input ([GUICommand], [Input])
processInput model0 = proc inp -> do
             rec
               (model, updateList) <- processUpdateCommands model0 -< (inp, pos)
               (newInput, fileList) <- processFileCommands -< (inp, model)
               (pos, moveList) <- processMoveCommands 0 -< (inp, model)
               dialogList <- processDialogCommands -< inp
               controlList <- processControlCommands -< inp
             id -< (concat [updateList, moveList, dialogList, fileList, controlList], newInput)

processUpdateCommands :: Model -> Auto IO (Input, Int) (Model, [GUICommand])
processUpdateCommands model0 = proc (inp, pos) -> do
                bupdates <- emitJusts getUpdates -< inp
                bmodelCmds <- perBlip (updateAuto model0) -< (,pos) <$> bupdates
                model <- holdWith_ model0 -< fst <$> bmodelCmds
                cmds <- fromBlips [] -< snd <$> bmodelCmds
                id -< (model, cmds)

processFileCommands :: Auto IO (Input, Model) ([Input], [GUICommand])
processFileCommands = proc (inp, model) -> do
                        bfiles <- emitJusts getFileCommands -< inp
                        bmodelCmds <- perBlip fileAuto -< (, model) <$> bfiles
                        let binputCmds = first (arr toInput) <$> bmodelCmds
                        fromBlips ([], []) -< binputCmds
                      where toInput Nothing = []
                            toInput (Just m) = [ InputUpdate (ChangeModel m)
                                               , InputMove MoveBegin ]

getUpdates :: Input -> Maybe UpdateCommand
getUpdates (InputUpdate cmd) = Just cmd
getUpdates _ = Nothing

getFileCommands :: Input -> Maybe FileCommand
getFileCommands (InputFile cmd) = Just cmd
getFileCommands _ = Nothing

processMoveCommands :: Int -> Auto IO (Input, Model) (Int, [GUICommand])
processMoveCommands pos0 = proc (inp, model) -> do
                             bmoves <- emitJusts getMoves -< inp
                             bposCmds <- perBlip (movementAuto pos0) -< (, model) <$> bmoves
                             pos <- holdWith_ pos0 -< fst <$> bposCmds
                             cmds <- fromBlips [] -< snd <$> bposCmds
                             id -< (pos, cmds)

getMoves :: Input -> Maybe MoveCommand
getMoves (InputMove cmd) = Just cmd
getMoves _ = Nothing

processDialogCommands :: Auto IO Input [GUICommand]
processDialogCommands = proc inp -> do
                    b <- emitJusts getDialogs -< inp
                    ds <- perBlip dialogAuto -< b
                    fromBlips [] -< ds

getDialogs :: Input -> Maybe DialogCommand
getDialogs (InputDialog cmd) = Just cmd
getDialogs _ = Nothing

processControlCommands :: Auto IO Input [GUICommand]
processControlCommands = proc inp -> do
                           b <- emitJusts getControls -< inp
                           cmds <- perBlip controlAuto -< b
                           fromBlips [] -< cmds

getControls :: Input -> Maybe ControlCommand
getControls (InputControl cmd) = Just cmd
getControls _ = Nothing
