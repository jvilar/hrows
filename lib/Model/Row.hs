module Model.Row ( Row
                 , emptyRow
                 , DataSource
                 , fillEmpty
                 ) where

import Model.Field

-- |A row is a list of fields.
type Row = [Field]

-- |An empty 'Row'.
emptyRow :: Row
emptyRow = []

-- |The data sources are lists of rows for recovering fields.
type DataSource = [Row]

-- |Fill a `Row` with empty fields.
fillEmpty :: Row -> Row
fillEmpty = (++ repeat (toField ()))
