{-# LANGUAGE Arrows, TupleSections #-}

module Presenter (
                 -- *Functions
                 presenter
                 -- *Reexported
                 , module Input
) where

import Control.Arrow(arr, first)
import Control.Auto(Auto, accum_, accumM_, arrM, delay_, fromBlips, emitJusts, holdWith_, perBlip, stepAuto, id, (.))
import Control.Monad(foldM)
import Data.Maybe(fromMaybe, isJust)
import Data.Monoid((<>))
import Prelude hiding((.), id)

import GUICommand
import Input
import Model

-- Ugly hack. We want that after a load file there is a movement to
-- the begin of the model. To do so, presenter first transform its
-- input into a list of Input where the first is the actual input and
-- the second may come derived from it. E.g. (InputFile (LoadFile
-- info)) gets transformed into [InputFile (LoadFile info), InputMove
-- MoveBegin]. Then the list is processed.
presenter :: Model -> Auto IO Input [GUICommand]
presenter model0 = proc inp -> do
                     bfile <- emitJusts getFileCommands -< inp
                     bother <- emitJusts getNonFileCommands -< inp
                     inputs <- fromBlips [] -< ((:[]) <$> bother) <> (addMovement <$> bfile)
                     arrM print -< inputs
                     -- (cmds, _) <- accumM_ update ([], processInput model0) -< inputs
                     cmds <- updater model0 -< inputs
                     arrM putStrLn -< "Commands: " ++ show cmds
                     id -< cmds


updater :: Model -> Auto IO [Input] [GUICommand]
updater model0 = proc inputs -> do
    rec
        dauto <- delay_ (processInput model0) -< auto
        (cmds, auto) <- arrM (uncurry pr) -< (dauto, inputs)
    id -< cmds

pr :: Auto IO Input [GUICommand] -> [Input] -> IO ([GUICommand], Auto IO Input [GUICommand])
pr auto [] = return ([], auto)
pr auto (i:is) = do
    (cmds, auto') <- stepAuto auto i
    (cmds', auto'') <- pr auto' is
    return $ (cmds ++ cmds', auto'')

update :: ([GUICommand], Auto IO Input [GUICommand])
                 -> [Input] -> IO ([GUICommand], Auto IO Input [GUICommand])
update (_, auto0) = foldM step ([], auto0)
    where step (cmds, auto) i = do
              print i
              (cmds', auto') <- stepAuto auto i
              return (cmds ++ cmds', auto')

addMovement :: FileCommand -> [Input]
addMovement c@(LoadFile _) = [InputFile c, InputMove MoveBegin]
addMovement c@(LoadFileFromName _) = [InputFile c, InputMove MoveBegin]
addMovement c = [InputFile c]

-- |The presenter admits inputs and produces information
-- for updating the display.
processInput :: Model -> Auto IO Input [GUICommand]
processInput model0 = proc inp -> do
             rec
               (model1, updateList) <- processUpdateCommands model0 -< (inp, pos)
               (model2, fileList) <- processFileCommands -< (inp, model)
               model <- arr (uncurry fromMaybe) -< (model1, model2)
               (pos, moveList) <- processMoveCommands 0 -< (inp, model)
               arrM print -< "---------------------------------"
               arrM print -< (model1, model2, model)
               arrM putStrLn -< "Position: " ++ show pos
               arrM print -< "---------------------------------"
               dialogList <- processDialogCommands -< inp
             id -< concat [updateList, moveList, dialogList, fileList]

processUpdateCommands :: Model -> Auto IO (Input, Int) (Model, [GUICommand])
processUpdateCommands model0 = proc (inp, pos) -> do
                bupdates <- emitJusts getUpdates -< inp
                bmodelCmds <- perBlip (updateAuto model0) -< (,pos) <$> bupdates
                model <- holdWith_ model0 -< fst <$> bmodelCmds
                arrM putStrLn -< "En updateCommands, model: " ++ show model
                cmds <- fromBlips [] -< snd <$> bmodelCmds
                id -< (model, cmds)

processFileCommands :: Auto IO (Input, Model) (Maybe Model, [GUICommand])
processFileCommands = proc (inp, model) -> do
                        bfiles <- emitJusts getFileCommands -< inp
                        arrM print -< "--------"
                        arrM print -< bfiles
                        bmodelCmds <- perBlip fileAuto -< (, model) <$> bfiles
                        arrM (print . (first isJust <$>)) -< bmodelCmds
                        fromBlips (Nothing, []) -< bmodelCmds

getUpdates :: Input -> Maybe UpdateCommand
getUpdates (InputUpdate cmd) = Just cmd
getUpdates _ = Nothing

getFileCommands :: Input -> Maybe FileCommand
getFileCommands (InputFile cmd) = Just cmd
getFileCommands _ = Nothing

getNonFileCommands :: Input -> Maybe Input
getNonFileCommands (InputFile cmd) = Nothing
getNonFileCommands c = Just c


processMoveCommands :: Int -> Auto IO (Input, Model) (Int, [GUICommand])
processMoveCommands pos0 = proc (inp, model) -> do
                             arrM putStrLn -< "En moveCommands, model: " ++ show model
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
