module View.ArticleView

where

import Entity.DetailArticle
import Config
import Lucid
import qualified View.Template.ArticleTemplate as TMP

data ArticleView = ArticleView
  { article :: DetailArticle
  , conf :: Config
  }

instance ToHtml ArticleView where
  toHtml av = TMP.render ctx
    where
      ar = article av
      cfg = conf av
      ctx = TMP.Context
        { TMP.title = title ar
        , TMP.markdown = markdown ar
        , TMP.slug = slug ar
        , TMP.retz = released_at ar
        , TMP.uptz = updated_at ar
        , TMP.baseUrl = baseUrl $ extConfig cfg
        , TMP.ogpImage = ogpImage $ extConfig cfg
        , TMP.xogpImage = ogpImageForX $ extConfig cfg
        , TMP.xaccount = accountForX $ extConfig cfg
        , TMP.siteName = blogTitle $ extConfig cfg
        , TMP.locale = locale $ extConfig cfg
        , TMP.articleDir = articleDir $ extConfig cfg
        }
      
