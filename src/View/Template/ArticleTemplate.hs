{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module View.Template.ArticleTemplate where

import Lucid
import Lucid.Base
import Data.Text as T
import Data.Time (ZonedTime)
import qualified Text.MMark as M
import Utils (tz2text)

{-
   HTML Template for DetailArticle
-}

data Context = Context
  { title    :: Text
  , markdown :: Text
  , slug     :: Text
  , retz     :: Maybe ZonedTime
  , uptz     :: ZonedTime
  , baseUrl  :: Text
  , ogpImage :: Text
  , xogpImage :: Maybe Text
  , xaccount :: Maybe Text
  , siteName :: Text
  , articleDir :: Maybe Text
  , locale :: Text
  }
-- | render HTML
-- 
-- 
render :: Monad m => Context -> HtmlT m ()
render ctx =
  doctype_ <> headTag <> bodyTag
  where
    headTag = head_ $ do
      title_ $ toHtml ctx.title 
      meta_ [name_ "description", content_ $ T.take 50 ctx.markdown <> "..."]
      -- OGP DATA
      meta_ [property_ "og:title", content_ ctx.title]
      meta_ [property_ "og:site_name", content_ ctx.siteName]
      meta_ [property_ "og:description", content_ $ T.take 50 ctx.markdown <> "..."]
      meta_ [property_ "og:type", content_ "article"]
      case ctx.articleDir of
        Nothing -> meta_ [property_ "og:url", content_ (ctx.baseUrl <> "/posts/" <> ctx.slug)]
        Just dir -> meta_ [property_ "og:url", content_ (ctx.baseUrl <> "/" <> dir <> "/" <> ctx.slug)]
      meta_ [property_ "og:locale", content_ ctx.locale]
      meta_ [property_ "og:image", content_ ctx.ogpImage]
      -- OGP for Twitter
      meta_ [name_ "twitter:title", content_ ctx.title]
      meta_ [name_ "twitter:card", content_ "summary"]
      case ctx.xaccount of
        Nothing -> pure ()
        Just ac -> meta_ [name_ "twitter:site", content_ ac]
      case ctx.xogpImage of
        Nothing -> pure ()
        Just xogp -> meta_ [name_ "twitter:image", content_ xogp]
      meta_ [name_ "twitter:description", content_ $ T.take 50 ctx.markdown <> "..."]
    bodyTag = body_ $ do
      h1_ $ toHtml ctx.title
      case ctx.retz of
        Nothing -> p_ ""
        Just re -> p_ $ toHtml ("Publish: " <> tz2text re)
      p_ $ toHtml ("Update: " <> tz2text ctx.uptz)
      -- parse markdown
      case M.parse "" ctx.markdown of
        Left bundle -> p_ $ toHtml ctx.markdown
        Right r -> toHtmlRaw $ show $ M.render r
    property_ = makeAttributes "property"

