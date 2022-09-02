module Presenter.Listing (
  -- *Types
  ListingCommand(..)
) where

data ListingCommand = ShowListingRequested
                    | CloseListingRequested
                    | CompleteListingGranted deriving Show
