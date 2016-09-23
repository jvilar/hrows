module Message (
    -- *Types
    Message(..)
    ) where

data Message = ErrorMessage String
             | WarningMessage String
             | InformationMessage String
             | QuestionMessage String
             deriving Show
