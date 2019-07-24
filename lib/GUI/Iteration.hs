module GUI.Iteration (
    -- *Types
    Iteration(..)
    -- *Reexported modules
    , module Message
    ) where

import Data.Text(Text)

import Model
import Message
import Presenter.ImportType

-- |The possible iterations with the user.
data Iteration = NoIteration -- ^No need to iterate.
               | AskReadFile -- ^Ask for a file name to read.
               | AskWriteFile -- ^Ask for a file name to write.
               | AskCreateField -- ^Ask for fields to create.
               | AskDeleteFields [Name] -- ^Ask for fields to delete.
               | AskImportFrom ImportType -- ^Ask for a file to import from.
               | AskImportOptions ImportType [Name] [Name] Model -- ^Ask for the pairing of fields in an import.
               | AskRenameFields [Name] -- ^Ask for fields to rename.
               | AskSortRows [Name] -- ^Ask the field for sorting.
               | DisplayMessage Message -- ^Display a message.
               | ConfirmExit Bool -- ^Confirm exit program.
               | GetFieldFormula FieldPos Name (Maybe Formula) -- ^Introduce the formula for a field
               | SearchField FieldPos Text [Text] -- ^Search for a record containing a value
               | CopyOtherField FieldPos Text [Text] -- ^Copy the value of the field from another record
               deriving Show
