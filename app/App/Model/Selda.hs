module App.Model.Selda
  ( module App.Model.Selda,
    module Database.Selda,
    module Database.Selda.SqlType,
    module Database.Selda.PostgreSQL,
    module Database.Selda.Backend,
  )
where

import Data.Maybe
import Data.Text
import Database.Selda hiding (def)
import Database.Selda.Backend
import Database.Selda.PostgreSQL
import Database.Selda.SqlType
import Text.Casing

-- | Convert record field name to database table field name.
toFieldName p = toSnakeCase . fromJust . stripPrefix p

toSnakeCase = toLower . pack . toSnake . fromHumps . unpack
