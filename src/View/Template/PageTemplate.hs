{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module View.Template.PageTemplate where

import Lucid
import Lucid.Base
import Data.Text as T
import qualified Text.MMark as M

{-
   HTML rnder template for Page
-}

data Context = Context
  { title :: Text
  , markdown :: Text
  , slug :: Text
  , baseUrl :: Text
  , siteName :: Text
  , ogpImage :: Text
  , xogpImage :: Maybe Text
  , xaccount :: Maybe Text
  , locale :: Text
  }

-- | render HTML
--
-- args: (title, markdown, slug)
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
      meta_ [property_ "og:type", content_ "website"]
      meta_ [property_ "og:url", content_ (ctx.baseUrl <> "/" <> ctx.slug)]
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
        Just x -> meta_ [name_ "twitter:image", content_ x]
      meta_ [name_ "twitter:description", content_ $ T.take 50 ctx.markdown <> "..."]
    bodyTag = body_ $ do
      h1_ $ toHtml ctx.title
      -- parse markdown
      case M.parse "" ctx.markdown of
        Left bundle -> p_ $ toHtml ctx.markdown
        Right r -> toHtmlRaw $ show $ M.render r
    property_ = makeAttributes "property"

