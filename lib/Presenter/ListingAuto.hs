module Presenter.ListingAuto (
  listingAuto
) where

import Control.Auto (arrM)

import GUI.Command
import Presenter.Auto (PresenterAuto, PresenterM, sendGUIM)
import Presenter.Listing (ListingCommand(..))

listingAuto :: PresenterAuto ListingCommand ()
listingAuto = arrM processCommand

processCommand :: ListingCommand -> PresenterM ()
processCommand ShowListingRequested = sendGUIM ShowListing
processCommand CloseListingRequested = sendGUIM HideListing
processCommand (ListingFilterChanged t) = undefined
