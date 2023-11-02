{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database.Entity.Tag where

import GHC.Generics ( Generic )
import Data.Text ( Text )
import Data.UUID.Types ( UUID )
import Database.PostgreSQL.Simple(ToRow, FromRow)

data DBTag = DBTag
  { id :: UUID
  , slug :: Text
  , name :: Text
  } deriving(Generic, ToRow, Show, FromRow)