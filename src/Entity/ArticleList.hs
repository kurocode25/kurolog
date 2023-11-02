{-# LANGUAGE DeriveGeneric #-}
module Entity.ArticleList (ArticleList (..))
where

import Entity.Article as Article
import Entity.Pagination ( Pagination )
import GHC.Generics (Generic)
import Data.Aeson

data ArticleList = ArticleList
  { articles :: [Article]
  , pagination :: Pagination
  } deriving (Show, Generic)

instance ToJSON ArticleList
    
