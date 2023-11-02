{-# LANGUAGE DeriveGeneric #-}

module Entity.DetailArticle (DetailArticle(..)) where

import GHC.Generics
import Data.Text

import Entity.Category
import Entity.Tag
import Data.Aeson (ToJSON)
import Data.UUID.Types (UUID)
import Data.Time (ZonedTime)

data DetailArticle = DetailArticle
  { id :: UUID
  , slug :: Text
  , status :: Int
  , title :: Text
  , released_at :: Maybe ZonedTime
  , updated_at :: ZonedTime
  , image :: Maybe Text
  , category :: Category
  , tags :: [Tag]
  , markdown :: Text
  } deriving(Show, Generic)

instance ToJSON DetailArticle

