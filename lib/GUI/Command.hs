module GUI.Command(
                  GUICommand(..)
                  , FieldInfo(..)
                  , module GUI.Iteration
                  ) where


import Data.Text(Text)
import GUI.Iteration

import Model

-- |The commands that are interpreted by the GUI
data GUICommand = ChangeTitle Text -- ^Change the title of the window
                | ShowPosition Int Int -- ^Show he position and the number of rows
                | ShowFields RowPos [FieldInfo] -- ^Show the position and the fields in that position
                | ShowNames [FieldName] -- ^Show the names of the fields
                | ShowIteration Iteration -- ^Open an iteration with the user
                | ShowFilterOK -- ^Show that the filter is OK
                | ShowFilterError -- ^Show that the filter has a problem
                | DisableTextViews -- ^Do not allow text input
                | ShowListing -- ^Show the listing window
                | HideListing -- ^Hide the listing window
                | CompleteListingWanted -- ^Hint the GUI that the listing needs to be completely refreshed.
                                        --  If the listing window is not hidden, the GUI will send a `CompleteListingGranted`
                                        --  message
                | CompleteListing [[Text]] -- ^The complete listing information, sent when a `CompleteListingGranted` is received
                deriving Show

-- |The information need to show the field
data FieldInfo = FieldInfo { indexFI :: FieldPos
                           , textFI :: Text
                           , formulaFI :: Maybe Formula
                           , typeFI :: FieldType
                           , isErrorFI :: Bool
                           , isVisibleFI :: Bool
                           } deriving Show
