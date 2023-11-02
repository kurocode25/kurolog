module View.PageView
where

import Entity.Page as P
import Config as C
import Lucid
import View.Template.PageTemplate as TMP

data PageView = PageView
  { page :: Page
  , config :: Config
  }

instance ToHtml PageView where
  toHtml v = TMP.render ctx
    where
      conf = config v
      p = page v
      ext = extConfig conf
      ctx = Context
        { TMP.title = P.title p
        , TMP.markdown = P.markdown p
        , TMP.slug = P.slug p
        , TMP.baseUrl = C.baseUrl ext
        , TMP.siteName = blogTitle ext
        , TMP.ogpImage = C.ogpImage ext
        , TMP.xogpImage = ogpImageForX ext
        , TMP.xaccount = accountForX ext
        , TMP.locale = C.locale ext
        }
