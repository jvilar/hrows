module Iteration (
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
               | DisplayMessage Message -- ^Display a message.
               deriving Show
