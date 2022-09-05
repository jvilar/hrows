module Presenter.ListingAuto (
  listingAuto
) where

import Control.Auto (arrM)
import Data.Text (Text)

import GUI.Command
import Presenter.Auto (PresenterAuto, PresenterM, sendGUIM, Filter)
import Presenter.Listing (ListingCommand(..))
import Model (Model, from, rows, toString, toInt, getDataSources, (<@), isError)
import Model.Expression.Evaluation (evaluate)
import Control.Monad (guard)
import Model.Field (typeOf)
import Model.Field (FieldType(TypeInt))

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
generateCells model Nothing = Just [[toString field | field <- row] | row <- rows `from` model]
generateCells model (Just (expr, _)) = sequenceA $ do
  row <- rows `from` model
  let val = evaluate row (getDataSources <@ model) expr
      valid = typeOf val == TypeInt && toInt val > 0
  guard $ isError val || valid
  return $ if isError val
           then Nothing
           else Just [toString field | field <- row]
