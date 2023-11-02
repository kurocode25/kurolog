module Mapper
where

import Entity.Article as A
import Entity.Category as C
import Entity.Tag as T
import Entity.ArticleList as AL
import Entity.Pagination
import Entity.DetailArticle as ED
import Entity.RequestArticle as ER
import Entity.Page as EP
import Entity.User as EU

import Database.Entity.Article as DEA
import Database.Entity.Tag as DET ( DBTag(slug, id, name) )
import Database.Entity.Category as DEC
import Database.Entity.DBUser as DEDU
import qualified Database.Entity.DBPage as DEP (DBPage(..))

import Utils

{-
   Mapper module has functions for convert from Database Entity to API Entity
-}

-- | mapping Page from DBPage
toPage :: DEP.DBPage -> EP.Page
toPage dp =
  EP.Page { EP.id = Just $ DEP.id dp
          , EP.title = DEP.title dp
          , EP.slug = DEP.slug dp
          , EP.markdown = DEP.markdown dp
          , EP.created_at = Just $ tzstamp2tz $ DEP.created_at dp
          , EP.updated_at = Just $ tzstamp2tz $ DEP.updated_at dp
          , EP.pub_flag = DEP.pub_flag dp
          }

toArticle :: DBArticle -> Article
toArticle da = Article {
  A.id = DEA.id da,
  A.slug = DEA.slug da,
  A.status = DEA.status da,
  A.title = DEA.title da,
  A.released_at = convertReleasedAtTime $ DEA.released_at da,
  A.updated_at = tzstamp2tz $ DEA.updated_at da,
  A.created_at = tzstamp2tz $ DEA.created_at da,
  A.image = DEA.image da,
  A.category = Category 
            { C.id = Just $ DEA.category_id da
            , C.slug = DEA.category_slug da
            , C.name = DEA.category_name da
            , C.created_at = Just $ tzstamp2tz $ DEA.category_created_at da
            , C.updated_at = Just $ tzstamp2tz $ DEA.category_updated_at da
            , C.last_posted_at = Just $ tzstamp2tz $ DEA.category_last_posted_at da
            , C.count = Nothing
            }
}

toArticleList :: [DBArticle] -> CurrentPage -> Limit -> Total -> ArticleList
toArticleList dbl c l totalItem =
  ArticleList
   { AL.articles = map toArticle dbl 
   , pagination = Pagination c (totalPages totalItem) next back l
   }
   where
    totalPages :: Int -> Int
    totalPages 0 = 1
    totalPages n = if n `mod` l > 0
      then n `div` l + 1
      else n `div` l
    next :: Maybe Int
    next = if totalPages totalItem > c
      then Just (c + 1)
      else Nothing
    back :: Maybe Int
    back = if c > 1
      then Just (c - 1)
      else Nothing

articleToNewArticle :: RequestArticle -> NewArticle
articleToNewArticle ar =
  NewArticle
    { DEA.newSlug = ER.slug ar
    , DEA.newStatus = ER.status ar
    , DEA.newCategoryId = ER.category_id ar
    , DEA.newTitle = ER.title ar
    , DEA.newImage = ER.image ar
    , DEA.newMarkdown = ER.markdown ar
    }

toDetailArticle :: DBArticle -> DetailArticle
toDetailArticle da = DetailArticle {
  ED.id = DEA.id da,
  ED.slug = DEA.slug da,
  ED.status = DEA.status da,
  ED.title = DEA.title da,
  ED.released_at = convertReleasedAtTime $ DEA.released_at da,
  ED.updated_at = tzstamp2tz $ DEA.updated_at da,
  ED.image = DEA.image da,
  ED.category = Category { C.id = Just $ DEA.category_id da
            , C.slug = DEA.category_slug da
            , C.name = DEA.category_name da
            , C.created_at = Just $ tzstamp2tz $ DEA.category_created_at da
            , C.updated_at = Just $ tzstamp2tz $ DEA.category_updated_at da
            , C.last_posted_at = Just $ tzstamp2tz $ DEA.category_last_posted_at da
            , C.count = Nothing
             },
  ED.tags = [],
  ED.markdown = DEA.markdown da
}

toTag :: DBTag -> Tag
toTag dt =
  Tag { T.id = Just $ DET.id  dt
      , T.name = DET.name dt
      , T.slug = DET.slug dt
      }

toCategory :: DBCategory -> Category
toCategory dc =
  Category { C.id = Just $ DEC.id dc
           , C.name = DEC.name dc
           , C.slug = DEC.slug dc
           , C.created_at = Just $ tzstamp2tz $ DEC.created_at dc
           , C.updated_at = Just $ tzstamp2tz $ DEC.updated_at dc
            , C.last_posted_at = Just $ tzstamp2tz $ DEC.last_posted_at dc
           , C.count = Just $ DEC.count dc
           }

toUser :: DBUser -> User
toUser du =
  User { EU.id  = Just $ DEDU.id du
       , EU.code = DEDU.code du
       , EU.name = DEDU.name du
       , EU.email = DEDU.email du
       , EU.password  = Nothing -- Don't publish password
       }

