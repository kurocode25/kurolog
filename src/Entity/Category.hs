{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Entity.Category (Category(..)) where

import GHC.Generics ( Generic )
import Data.Text ( Text )
import Data.Time
import Data.Aeson (ToJSON, FromJSON)
import Data.UUID.Types (UUID)
import View.Class
import Utils (tz2text)
import Config
import Control.Monad.Trans.Reader

data Category = Category
  { id :: Maybe UUID
  , slug :: Text
  , name :: Text
  , created_at :: Maybe ZonedTime
  , updated_at :: Maybe ZonedTime
  , last_posted_at :: Maybe ZonedTime
  , count :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Category
instance FromJSON Category

instance ToSitemap Category where
  serialize ca = do
    base <- baseUrl . extConfig <$> ask
    return ( base <> "/category/" <> slug ca
           , maybe "" tz2text $ last_posted_at ca
           , "monthly"
           )

