module Presenter.Listing (
  -- *Types
  ListingCommand(..)
) where

import Data.Text(Text)

data ListingCommand = ShowListingRequested
                    | CloseListingRequested
                    | CompleteListingGranted
                    | ListingFilterChanged Text deriving Show