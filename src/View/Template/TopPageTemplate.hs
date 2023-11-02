{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module View.Template.TopPageTemplate where

import Lucid
import Lucid.Base
import Data.Text
import Entity.Article as EA
import Entity.Category as EC

data Context = Context
  { title :: Text
  , baseUrl :: Text
  , siteName :: Text
  , ogpImage :: Text
  , xogpImage :: Maybe Text
  , xaccount :: Maybe Text
  , locale :: Text
  , description :: Text
  , arList :: [Article]
  }

render :: Monad m => Context -> HtmlT m ()
render ctx =
  doctype_ <> headTag <> bodyTag
  where
    headTag = head_ $ do
      title_ $ toHtml ctx.title 
      meta_ [name_ "description", content_ ctx.description]
      -- OGP DATA
      meta_ [property_ "og:title", content_ ctx.title]
      meta_ [property_ "og:site_name", content_ ctx.siteName]
      meta_ [property_ "og:type", content_ "blog"]
      meta_ [property_ "og:url", content_ ctx.baseUrl]
      meta_ [property_ "og:locale", content_ ctx.locale]
      meta_ [property_ "og:image", content_ ctx.ogpImage]
      -- OGP for Twitter
      meta_ [name_ "twitter:title", content_ ctx.title]
      meta_ [name_ "twitter:card", content_ "summary"]
      case ctx.xaccount of
        Nothing -> pure ()
        Just xac -> meta_ [name_ "twitter:site", content_ xac]
      case ctx.xogpImage of
        Nothing -> pure ()
        Just xogp -> meta_ [name_ "twitter:image", content_ xogp]
      meta_ [name_ "twitter:description", content_ ctx.description]
    bodyTag = body_ $ do
      h1_ $ toHtml ctx.title
      div_ $ mapM_ articleDiv ctx.arList

    property_ = makeAttributes "property"
    articleDiv :: Monad m => Article -> HtmlT m ()
    articleDiv ar = do
      a_ [href_ (ctx.baseUrl <> "/entry/" <> EA.slug ar)] $ do
        div_ $ span_ $ toHtml $ EA.title ar
        div_ $ span_ $ toHtml $ EC.name $ category ar
      
