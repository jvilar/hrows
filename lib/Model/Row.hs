module Model.Row ( Row
                 , emptyRow
                 , DataSource
                 ) where

import Model.Field

-- |A row is a list of fields.
type Row = [Field]

-- |An empty 'Row'
emptyRow :: Row
emptyRow = []

-- |The data sources are lists of rows for recovering fields
type DataSource = [Row]
