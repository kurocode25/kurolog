{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Entity.Article (Article(..)) where

import GHC.Generics
import Data.Text ( Text )

import Entity.Category
import Data.Aeson (ToJSON)
import Data.Time ( ZonedTime )
import Data.UUID.Types (UUID)
import Data.HashMap.Strict
import Xmlbf
import View.Class
import Utils (tz2text)
import Config
import Control.Monad.Trans.Reader
import Data.Maybe


data Article = Article
  { id :: UUID
  , slug :: Text
  , title :: Text
  , status :: Int
  , released_at :: Maybe ZonedTime
  , created_at :: ZonedTime
  , updated_at :: ZonedTime
  , image :: Maybe Text
  , category :: Category
  } deriving(Generic, Show)

instance ToJSON Article

instance ToXml Article where
  toXml = mkXml
    where 
      mkXml :: Article -> [Node]
      mkXml x = element "urls" (singleton "" "") []

instance ToSitemap Article where
  serialize ar = do
    c <-ask
    let dir = fromMaybe "posts" $ articleDir $ extConfig c
    let base = baseUrl $ extConfig c
    return ( base <> "/" <> dir <> "/" <> Entity.Article.slug ar
           , tz2text $ Entity.Article.updated_at ar
           , "monthly"
           )
