module Presenter.ListingAuto (
  listingAuto
) where

import Control.Auto (arrM)

import GUI.Command
import Presenter.Auto (PresenterAuto, PresenterM, sendGUIM)
import Presenter.Listing (ListingCommand(..))
import Model (Model, from, rows, toString)

listingAuto :: PresenterAuto (ListingCommand, Model) ()
listingAuto = arrM processCommand

processCommand :: (ListingCommand, Model) -> PresenterM ()
processCommand (ShowListingRequested, _) = do
                                             sendGUIM ShowListing
                                             sendGUIM CompleteListingWanted
processCommand (CloseListingRequested, _) = sendGUIM HideListing
processCommand (CompleteListingGranted, model) = let
     cells = [[toString field | field <- row] | row <- rows `from` model]
   in sendGUIM $ CompleteListing cells

