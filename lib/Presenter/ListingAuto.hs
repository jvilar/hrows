module Presenter.ListingAuto (
  listingAuto
) where

import Control.Auto (arrM)
import Control.Monad (guard)
import Data.Text (Text)

import GUI.Command
import Model (Model, from, rows, toString, toInt, getDataSources, (<@), isError, typeOf, visibilities)
import Model.Expression.Evaluation (evaluate)
import Model.Field (FieldType(TypeInt))
import Model.Row (Row)
import Presenter.Auto (PresenterAuto, PresenterM, sendGUIM, Filter)
import Presenter.Listing (ListingCommand(..))

listingAuto :: PresenterAuto (ListingCommand, Model, Filter) ()
listingAuto = arrM processCommand

processCommand :: (ListingCommand, Model, Filter) -> PresenterM ()
processCommand (ShowListingRequested, _, _) = do
                                                sendGUIM ShowListing
                                                sendGUIM CompleteListingWanted
processCommand (CloseListingRequested, _, _) = sendGUIM HideListing
processCommand (CompleteListingGranted, model, fltr) =
   case generateCells model fltr of
     Nothing -> do
                   sendGUIM $ CompleteListing []
                   sendGUIM ShowFilterError
     Just cells -> sendGUIM $ CompleteListing cells

generateCells :: Model -> Filter -> Maybe [[Text]]
generateCells model Nothing = Just $ map (generateRowCells model) $ rows `from` model
generateCells model (Just (expr, _)) = sequenceA $ do
  row <- rows `from` model
  let val = evaluate row (getDataSources <@ model) expr
      valid = typeOf val == TypeInt && toInt val > 0
  guard $ isError val || valid
  return $ if isError val
           then Nothing
           else Just $ generateRowCells model row

generateRowCells :: Model -> Row -> [Text]
generateRowCells model row = map (toString . fst)
                           $ filter snd
                           $ zip row (visibilities `from` model)
