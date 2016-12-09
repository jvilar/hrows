module GUI.Iteration (
    -- *Types
    Iteration(..)
    -- *Reexported modules
    , module Message
    ) where

import Message

-- |The possible iterations with the user.
data Iteration = NoIteration -- ^No need to iterate.
               | AskReadFile -- ^Ask for a file name to read.
               | AskWriteFile -- ^Ask for a file name to write.
               | AskCreateField -- ^Ask for fields to create.
               | AskDeleteFields [String] -- ^Ask for fields to delete.
               | AskRenameFields [String] -- ^Ask for fields to rename.
               | DisplayMessage Message -- ^Display a message.
               | ConfirmExit Bool -- ^Confirm exit program.
               | GetFieldFormula Int String (Maybe String) -- ^Introduce the formula for a field
               | SearchField Int String [String]
               deriving Show
