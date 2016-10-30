module Presenter.UpdateAuto (
    updateAuto
) where

import Control.Arrow(first)
import Control.Auto(Auto, accumM_)
import Control.Monad.Trans(liftIO)
import Data.Maybe(fromMaybe)

import GUI.Command
import Model
import Presenter.Auto
import Presenter.Input

updateAuto :: PresenterAuto (UpdateCommand, Int) Model
updateAuto = accumM_ update empty

update :: Model -> (UpdateCommand, Int) -> PresenterM Model
update model (UpdateField fpos v, pos) = do
    let r = row pos model'
        (model', changed) = changeField pos fpos v model
    sendGUIM . ShowFields $ do
                             c <- changed
                             let f = r !! c
                                 text = if c /= fpos
                                        then Just $ toString f
                                        else Nothing
                             return $ FieldInfo { indexFI = c
                                                , textFI = text
                                                , formulaFI = fieldFormula c model'
                                                , typeFI = fieldType c model'
                                                , isErrorFI = isError f
                                                }
    return model'
update _ (ChangeModel model, _) = do
    sendGUIM $ ShowNames (cnames model)
    sendInputM MoveBegin
    return model
update model (DoNothing, _) = return model
update model (NewRow, _) = do
    sendInputM MoveEnd
    return $ addEmptyRow model
update model (DeleteRow, pos) = do
    sendInputM $ MoveHere pos
    return $ deleteRow pos model
update model (NewFields l, pos) = do
    let model' = newFields (map (first Just) l) model
    sendGUIM $ ShowNames (cnames model')
    sendInputM $ MoveHere pos
    return model'
update model (DeleteFields fs, pos) = do
    let model' = deleteFields fs model
    sendGUIM $ ShowNames (cnames model')
    sendInputM $ MoveHere pos
    return model'
update model (MoveField f t, pos) = do
    let model' = moveField f t model
    sendGUIM $ ShowNames (cnames model')
    sendInputM $ MoveHere pos
    return model'
update model (ChangeFieldType t f, pos) = do
    sendInputM $ MoveHere pos
    return $ changeFieldType t f model
update model (ChangeFieldFormula mf f, pos) = do
    sendInputM $ MoveHere pos
    return $ changeFieldFormula mf f model
cnames :: Model -> [String]
cnames = map (++ ": ") . fnames
