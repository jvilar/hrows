module Message (
    -- *Types
    Message(..)
    ) where

import Data.Text(Text)

data Message = ErrorMessage Text
             | WarningMessage Text
             | InformationMessage Text
             | QuestionMessage Text
             deriving Show
