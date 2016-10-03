module Model.SourceInfo ( SourceInfo(..)
                        , FormatInfo(..)
                        , FormatInfoClass(..)
                        , changeFileName
                        , changeFormatInfo
                        , mkSourceInfo
                        , module Model.ListatabInfo
                        ) where


import Model.ListatabInfo

-- |The information about the source of the model. It contains
-- the possible file path and the options related to te format.
data SourceInfo = SourceInfo { siFilePath :: Maybe FilePath
                             , siFormat :: FormatInfo
                             } deriving Show

-- |The information about the format of the model.
data FormatInfo = NoFormatInfo
                | ListatabFormat ListatabInfo
                deriving Show

changeFileName :: FilePath -> SourceInfo -> SourceInfo
changeFileName p s = s { siFilePath = Just p }

changeFormatInfo :: FormatInfo -> SourceInfo -> SourceInfo
changeFormatInfo f s = s { siFormat = f }

mkSourceInfo :: FormatInfoClass i => Maybe FilePath -> i -> SourceInfo
mkSourceInfo p = SourceInfo p . toFormatInfo

class FormatInfoClass t where
    toFormatInfo :: t -> FormatInfo

instance FormatInfoClass () where
    toFormatInfo () = NoFormatInfo

instance FormatInfoClass ListatabInfo where
    toFormatInfo = ListatabFormat

