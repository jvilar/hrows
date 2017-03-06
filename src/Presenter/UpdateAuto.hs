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
update _ (ChangeModel model, _) =
    completeRefresh 0 model
update model (DoNothing, _) = return model
update model (NewRow, _) = do
    sendInputM MoveEnd
    return $ addEmptyRow model
update model (DeleteRow, pos) =
    partialRefresh pos $ deleteRow pos model
update model (SortRows f dir, _) = do
    partialRefresh 0 $ sortRows f dir model
update model (NewFields l, pos) =
    completeRefresh pos $ newFields (map (first Just) l) model
update model (DeleteFields fs, pos) =
    completeRefresh pos $ deleteFields fs model
update model (RenameFields names, pos) =
    completeRefresh pos $ renameFields names model
update model (ImportFieldsFromModel m keys values, pos) =
    partialRefresh pos $ importFields m keys values model
update model (MoveField f t, pos) = do
    completeRefresh pos $ moveField f t model
update model (ChangeFieldType t f, pos) =
    partialRefresh pos $ changeFieldType t f model
update model (ChangeFieldFormula mf f, pos) =
    partialRefresh pos $ changeFieldFormula mf f model

cnames :: Model -> [String]
cnames = map (++ ": ") . fnames

partialRefresh :: Int -> Model -> PresenterM Model
partialRefresh pos model = do
    sendInputM $ MoveHere pos
    return model

completeRefresh :: Int -> Model -> PresenterM Model
completeRefresh pos model = do
    sendGUIM $ ShowNames (cnames model)
    sendInputM $ MoveHere pos
    return model
