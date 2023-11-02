{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Entity.Category where

import GHC.Generics ( Generic )
import Data.Text ( Text )
import Data.UUID.Types ( UUID )
import Database.PostgreSQL.Simple(ToRow, FromRow)
import Database.PostgreSQL.Simple.Time (ZonedTimestamp)

-- | Category Entity for Database
data DBCategory = DBCategory
  { id :: UUID
  , slug :: Text
  , name :: Text
  , created_at :: ZonedTimestamp
  , updated_at :: ZonedTimestamp
  , last_posted_at :: ZonedTimestamp
  , count :: Int
  } deriving(Generic, ToRow, Show, FromRow)
