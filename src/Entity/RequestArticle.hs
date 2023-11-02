{-# LANGUAGE DeriveGeneric #-}

module Entity.RequestArticle where

import GHC.Generics
import Data.Aeson (FromJSON)
import Data.UUID.Types (UUID)
import Data.Text ( Text )
import Data.Time ( ZonedTime )

-- | Request object for create and update Article
--
-- 'operation' code :
--
-- 100 : create
--
-- 200 : update
--
-- 300 : publish
data RequestArticle = RequestArticle
  { id :: Maybe UUID
  , slug :: Text
  , title :: Text
  , status :: Int
  , released_at :: Maybe ZonedTime
  , image :: Maybe Text
  , category_id :: UUID
  , markdown :: Text
  , tags :: [UUID] -- if article has no tag, tags value is emplty array.
  , operation :: Int
  } deriving(Generic)
  
instance  FromJSON RequestArticle
