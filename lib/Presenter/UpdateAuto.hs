{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Presenter.UpdateAuto (
    updateAuto
) where

import Control.Arrow(first)
import Control.Auto(accumM_)
import qualified Data.Text as T

import GUI.Command
import Model
import Presenter.Auto
import Presenter.Input

data UndoZipper a = UndoZipper { undo :: [a]
               , current :: a
               , redo :: [a]
               }

back :: UndoZipper a -> Maybe (UndoZipper a)
back (UndoZipper (u:us) c rs) = Just $ UndoZipper us u (c:rs)
back _ = Nothing

forward :: UndoZipper a -> Maybe (UndoZipper a)
forward (UndoZipper us c (r:rs)) = Just $ UndoZipper (c:us) r rs
forward _ = Nothing

push :: UndoZipper a -> a -> UndoZipper a
push (UndoZipper us c _) m = UndoZipper (c:us) m []

type ZM = UndoZipper (Model, RowPos)

initialZM :: ZM
initialZM = UndoZipper [] (empty, 0) []

message :: Message -> PresenterM ()
message = sendGUIM . ShowIteration . DisplayMessage

updateAuto :: PresenterAuto (UpdateCommand, RowPos) Model
updateAuto = fst . current <$> accumM_ undoOrUpdate initialZM

undoOrUpdate :: ZM -> (UpdateCommand, RowPos) -> PresenterM ZM
undoOrUpdate zm (Undo, _) = case back zm of
                                Nothing -> do
                                    message $ InformationMessage "No puedo deshacer"
                                    return zm
                                Just zm' -> do
                                    let (model, pos) = current zm'
                                    _ <- completeRefresh pos model
                                    return zm'
undoOrUpdate zm (Redo, _) = case forward zm of
                                Nothing -> do
                                    message $ InformationMessage "No puedo rehacer"
                                    return zm
                                Just zm' -> do
                                    let (model, pos) = current zm'
                                    _ <- completeRefresh pos model
                                    return zm'
undoOrUpdate zm (BlockUndo, _) = return $ UndoZipper [] (current zm) []
undoOrUpdate zm (DoNothing, _) = return zm
undoOrUpdate zm p@(_, pos) = push zm . (,pos) <$> update (fst $ current zm) p

update :: Model -> (UpdateCommand, RowPos) -> PresenterM Model
update model (UpdateField fpos v, pos) = do
    let (rst', changed) = changeField pos fpos v <@ model
        model' = setStore rst' model
        r = row pos rst'
    sendGUIM . ShowFields pos $ do
                             c <- changed
                             let f = r !!! c
                             return $ FieldInfo { indexFI = c
                                                , textFI = toString f
                                                , formulaFI = fieldFormula c rst'
                                                , typeFI = fieldType c rst'
                                                , isErrorFI = isError f
                                                , mustWriteFI = c /= fpos
                                                }
    return model'
update _ (ChangeModel model, _) =
    completeRefresh 0 model
update model (DoNothing, _) = return model
update model (NewRow, _) = do
    sendInputM MoveEnd
    return $ addEmptyRow `inside` model
update model (DeleteRow, pos) =
    partialRefresh pos $ deleteRow pos `inside` model
update model (SortRows f dir, _) =
    partialRefresh 0 $ sortRows f dir `inside` model
update model (NewFields l, pos) =
    completeRefresh pos $ newFields (map (first Just) l) `inside` model
update model (DeleteFields fs, pos) =
    completeRefresh pos $ deleteFields fs `inside` model
update model (RenameFields names, pos) =
    completeRefresh pos $ renameFields names `inside` model
update model (ImportFieldsFromRowStore m keys values, pos) =
    partialRefresh pos $ importFields m keys values `inside` model
update model (ImportRowsFromRowStore m values, pos) =
    partialRefresh pos $ importRows m values `inside` model
update model (MoveField f t, pos) =
    completeRefresh pos $ moveField f t `inside` model
update model (ChangeFieldType t f, pos) =
    partialRefresh pos $ changeFieldType t f `inside` model
update model (ChangeFieldFormula mf f, pos) =
    partialRefresh pos $ changeFieldFormula mf f `inside` model
update model (SetUnchanged, _) = return $ setUnchanged `inside` model
update model (AddNewSource rst, _) = return $ addSource rst model

cnames :: RowStore -> [FieldName]
cnames = map (`T.append` ": ") . fnames

partialRefresh :: Int -> Model -> PresenterM Model
partialRefresh pos model = do
    sendInputM $ MoveHere pos
    sendGUIM CompleteListingWanted
    return model

completeRefresh :: Int -> Model -> PresenterM Model
completeRefresh pos model = do
    sendGUIM $ ShowNames (cnames <@ model)
    partialRefresh pos model
