{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Entity.Page where

import Data.Text
import Data.UUID.Types (UUID)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import View.Class
import Data.Time ( ZonedTime )
import Utils (tz2text)
import Config
import Control.Monad.Trans.Reader

data Page = Page
  { id :: Maybe UUID
  , slug :: Text
  , title :: Text
  , markdown :: Text
  , created_at :: Maybe ZonedTime
  , updated_at :: Maybe ZonedTime
  , pub_flag :: Bool
  } deriving (Show, Generic)

instance ToJSON Page
instance FromJSON Page

instance ToSitemap Page where
  serialize page = do
    base <- baseUrl . extConfig <$> ask
    return ( base <> "/" <> slug page
           , maybe "" tz2text $ updated_at page
           , "monthly"
           )

