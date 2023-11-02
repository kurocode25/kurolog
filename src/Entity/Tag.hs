{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Entity.Tag (Tag(..)) where

import GHC.Generics
import Data.Text
import Data.Aeson (ToJSON, FromJSON)
import Data.UUID.Types (UUID)
import Servant (HasStatus)

data Tag = Tag
  { id :: Maybe UUID
  , slug :: Text
  , name :: Text
  } deriving(Show, Generic)

instance ToJSON Tag
instance FromJSON Tag

