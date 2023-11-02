{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database.Entity.Article where

import GHC.Generics
import Data.Text
import Data.UUID.Types
import Database.PostgreSQL.Simple(ToRow, FromRow)
import Database.PostgreSQL.Simple.Time (ZonedTimestamp)

-- | Object for get 'Artcile' data from database
data DBArticle = DBArticle
  { id :: UUID
  , slug :: Text
  , title :: Text
  , released_at :: Maybe ZonedTimestamp
  , created_at :: ZonedTimestamp
  , updated_at :: ZonedTimestamp
  , status :: Int
  , image :: Maybe Text
  , markdown :: Text
  , category_id :: UUID
  , category_slug :: Text
  , category_name :: Text
  , category_created_at :: ZonedTimestamp
  , category_updated_at :: ZonedTimestamp
  , category_last_posted_at :: ZonedTimestamp
  } deriving(Generic, ToRow, Show, FromRow)

-- | Object for create 'Article'
data NewArticle = NewArticle
  { newSlug :: Text
  , newStatus :: Int
  , newCategoryId :: UUID
  , newTitle :: Text
  , newImage :: Maybe Text
  , newMarkdown :: Text
  } deriving (Generic, ToRow, Show)

-- | Objcet for update Article
--
--  (At this point, this object is not used.)
data UpdateArticle = UpdateArticle
  { updateTitle :: Text
  , updateStatus :: Int
  , updateImage :: Maybe Text
  , updateMarkdown :: Text
  }

