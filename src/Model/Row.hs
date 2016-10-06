module Model.Row ( Row
                 , emptyRow
                 ) where

import Model.Field

-- |A row is a list of fields.
type Row = [Field]

-- |An empty 'Row'
emptyRow :: Row
emptyRow = []
