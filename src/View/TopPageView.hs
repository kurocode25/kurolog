{-# LANGUAGE OverloadedStrings #-}
module View.TopPageView where

import Data.Maybe
import Entity.Article
import View.Template.TopPageTemplate as TMP
import Lucid
import Config as C

data TopPageView = TopPageView
  { articles :: [Article]
  , config :: Config
  }

instance ToHtml TopPageView where
  toHtml view = TMP.render ctx
    where
      ext = extConfig $ config view
      ar = articles view
      ctx = Context
        { TMP.title = blogTitle ext
        , TMP.baseUrl = C.baseUrl ext
        , TMP.siteName = C.blogTitle ext
        , TMP.ogpImage = C.ogpImage ext
        , TMP.xogpImage = C.ogpImageForX ext
        , TMP.xaccount = C.accountForX ext
        , TMP.locale = C.locale ext
        , TMP.description = C.description ext
        , TMP.arList = ar
        , TMP.articleDir = fromMaybe "posts" $ C.articleDir ext
        }

