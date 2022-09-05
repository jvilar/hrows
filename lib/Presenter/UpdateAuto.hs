{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Presenter.UpdateAuto (
    updateAuto
) where

import Control.Arrow(first)
import Control.Auto(accumM_)
import Control.Exception(throwIO)
import Control.Monad.IO.Class(liftIO)
import Data.Text(Text)
import qualified Data.Text as T

import GUI.Command
import HRowsException
import Model
import Presenter.Auto
import Presenter.Input
import Model.Expression.Parser (expression, eof, parse)
import Model.Expression.Manipulate (addPositions)

data UndoZipper a = UndoZipper [a] a [a]

current :: UndoZipper a -> a
current (UndoZipper _ c _) = c

back :: UndoZipper a -> Maybe (UndoZipper a)
back (UndoZipper (u:us) c rs) = Just $ UndoZipper us u (c:rs)
back _ = Nothing

forward :: UndoZipper a -> Maybe (UndoZipper a)
forward (UndoZipper us c (r:rs)) = Just $ UndoZipper (c:us) r rs
forward _ = Nothing

push :: UndoZipper a -> a -> UndoZipper a
push (UndoZipper us c _) m = UndoZipper (c:us) m []

type ModelFilter = (Model, Filter)
type ZM = UndoZipper (ModelFilter, RowPos)

initialZM :: ZM
initialZM = UndoZipper [] ((empty, Nothing), 0) []

message :: Message -> PresenterM ()
message = sendGUIM . ShowIteration . DisplayMessage

updateAuto :: PresenterAuto (UpdateCommand, RowPos) ModelFilter
updateAuto = fst . current <$> accumM_ undoOrUpdate initialZM

undoOrUpdate :: ZM -> (UpdateCommand, RowPos) -> PresenterM ZM
undoOrUpdate zm (Undo, _) = tryToMoveZM zm back "No puedo deshacer"
undoOrUpdate zm (Redo, _) =  tryToMoveZM zm forward "No puedo rehacer"
undoOrUpdate zm (BlockUndo, _) = return $ UndoZipper [] (current zm) []
undoOrUpdate zm (DoNothing, _) = return zm
undoOrUpdate zm p@(_, pos) = push zm . (,pos) <$> update (fst $ current zm) p

tryToMoveZM :: ZM -> (ZM -> Maybe ZM) -> Text -> PresenterM ZM
tryToMoveZM zm dir m = case dir zm of
                           Nothing -> do
                               message $ InformationMessage m
                               return zm
                           Just zm' -> do
                               let (modelFilter, pos) = current zm'
                               _ <- completeRefresh pos modelFilter
                               return zm'

insideF :: (RowStore -> RowStore) -> (Model, d) -> (Model, d)
insideF = first . inside

update :: ModelFilter -> (UpdateCommand, RowPos) -> PresenterM ModelFilter
update (model, flter) (UpdateField fpos v, pos) = do
    let (rst', chngd) = changeField pos fpos v <@ model
        model' = setStore rst' model
        r = row pos rst'
    sendGUIM . ShowFields pos $ do
                             c <- chngd
                             let f = r !!! c
                             return $ FieldInfo { indexFI = c
                                                , textFI = toString f
                                                , formulaFI = fieldFormula c rst'
                                                , typeFI = fieldType c rst'
                                                , isErrorFI = isError f
                                                , isVisibleFI = isVisible c rst'
                                                }
    return (model', flter)
update (_, flter) (ChangeModel model, _) =
    completeRefresh 0 (model, flter)
update (model, _) (ChangeFilter filterExpression, _) = do
    flter <- processFilter model filterExpression
    return (model, flter)
update modelFilter (DoNothing, _) = return modelFilter
update modelFilter (NewRow, _) = do
    sendInputM MoveEnd
    return $ addEmptyRow `insideF` modelFilter
update modelFilter (DeleteRow, pos) =
    partialRefresh pos $ deleteRow pos `insideF` modelFilter
update modelFilter (SortRows f dir, _) =
    partialRefresh 0 $ sortRows f dir `insideF` modelFilter
update modelFilter (NewFields l, pos) =
    completeRefresh pos $ newFields (map (first Just) l) `insideF` modelFilter
update modelFilter (DeleteFields fs, pos) =
    completeRefresh pos $ deleteFields fs `insideF` modelFilter
update modelFilter (RenameFields ns, pos) =
    completeRefresh pos $ renameFields ns `insideF` modelFilter
update modelFilter (HideField fpos, pos) =
    completeRefresh pos $ hideField fpos `insideF` modelFilter
update modelFilter (SetFieldsVisibility vs, pos) =
    completeRefresh pos $ changeVisibleFields vs `insideF` modelFilter
update modelFilter (ImportFieldsFromRowStore m keys values, pos) =
    partialRefresh pos $ importFields m keys values `insideF` modelFilter
update modelFilter (ImportRowsFromRowStore m values, pos) =
    partialRefresh pos $ importRows m values `insideF` modelFilter
update modelFilter (MoveField f t, pos) =
    completeRefresh pos $ moveField f t `insideF` modelFilter
update modelFilter (ChangeFieldType t f, pos) =
    partialRefresh pos $ changeFieldType t f `insideF` modelFilter
update modelFilter (ChangeFieldFormula mf f, pos) =
    partialRefresh pos $ changeFieldFormula mf f `insideF` modelFilter
update modelFilter (SetUnchanged, _) = return $ setUnchanged `insideF` modelFilter
update modelFilter (AddNewSource si rst, _) = do
    message . InformationMessage $ T.concat ["Se ha añadido la fuente ", getName rst]
    return $ first (addSource si rst) modelFilter
update modelFilter (RenameSources ns, pos) =
    partialRefresh pos $ first (renameSources ns) modelFilter
update modelFilter (DeleteSources ns, pos) =
    partialRefresh pos $ first (deleteSources ns) modelFilter
update _ (Undo, _) = liftIO $ throwIO $ HRowsException "No puede llegar un Undo al método update de UpdateAuto"
update _ (Redo, _) = liftIO $ throwIO $ HRowsException "No puede llegar un Redo al método update de UpdateAuto"
update _ (BlockUndo, _) = liftIO $ throwIO $ HRowsException "No puede llegar un BlockUndo al método update de UpdateAuto"

processFilter :: Model -> Formula -> PresenterM Filter
processFilter _ formula | T.null formula = do
    sendGUIM ShowFilterOK
    sendGUIM CompleteListingWanted
    return Nothing
processFilter model formula = do
    expr <- case parse (expression <* eof) formula of
               Left err -> do
                             sendGUIM ShowFilterError
                             return $ mkErrorExpr err
               Right expr -> do
                               sendGUIM ShowFilterOK
                               return $ flip addPositions expr `from` model
    sendGUIM CompleteListingWanted
    return $ Just (expr, formula)

cnames :: RowStore -> [FieldName]
cnames = map (`T.append` ": ") . fnames

partialRefresh :: Int -> ModelFilter -> PresenterM ModelFilter
partialRefresh pos modelFilter = do
    sendInputM $ MoveHere pos
    sendGUIM CompleteListingWanted
    return modelFilter

completeRefresh :: Int -> ModelFilter -> PresenterM ModelFilter
completeRefresh pos modelFilter@(model, _) = do
    sendGUIM $ ShowNames (cnames <@ model)
    partialRefresh pos modelFilter
