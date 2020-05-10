module GUI.Command(
                  GUICommand(..)
                  , FieldInfo(..)
                  , module GUI.Iteration
                  ) where


import Data.Text(Text)
import GUI.Iteration

import Model

data GUICommand = ChangeTitle Text
                | ShowPosition Int Int
                | ShowFields RowPos [FieldInfo]
                | ShowNames [FieldName]
                | ShowIteration Iteration
                | DisableTextViews
                | ShowListing
                | HideListing
                | CompleteListingWanted
                | CompleteListing [[Text]]
                deriving Show

data FieldInfo = FieldInfo { indexFI :: FieldPos
                           , textFI :: Text
                           , formulaFI :: Maybe Formula
                           , typeFI :: FieldType
                           , isErrorFI :: Bool
                           , mustWriteFI :: Bool
                           } deriving Show
