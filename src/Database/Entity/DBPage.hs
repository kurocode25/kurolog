{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database.Entity.DBPage (DBPage(..)) where

import Data.Text
import Data.UUID.Types (UUID)
import GHC.Generics
import Database.PostgreSQL.Simple(ToRow, FromRow)
import Database.PostgreSQL.Simple.Time (ZonedTimestamp)

data DBPage = DBPage
  { id :: UUID
  , slug :: Text
  , title :: Text
  , markdown :: Text
  , created_at :: ZonedTimestamp
  , updated_at :: ZonedTimestamp
  , pub_flag :: Bool
  } deriving (Show, ToRow, FromRow, Generic)
