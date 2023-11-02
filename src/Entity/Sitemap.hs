{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Entity.Sitemap
where

import Data.Text as DT
import Data.Text.Lazy as DTL
import Entity.Article as EA
import Entity.Category as EC
import Xmlbf
import Data.HashMap.Strict as DHS
import Data.List as DL
import View.Class
import Entity.Page
import Config
import Utils
import Control.Monad.Trans.Reader

data Sitemap = Sitemap
  { articles :: [Article]
  , categories :: [Category]
  , pages :: [Page]
  , config :: Config
  } deriving (Show)

instance ToXml Sitemap where
  toXml :: Sitemap -> [Node]
  toXml su = parent
    where
      parent :: [Node]
      parent = urlset
        (  DL.foldl' (++) [] (mkNodeList $ articles su)
        -- ++ DL.foldl' (++) [] (mkNodeList $ categories su)
        ++ DL.foldl' (++) [] (mkNodeList $ pages su)
        -- TOP PAGE
        ++ element "url" DHS.empty (mkNode (topUrl $ config su, lastUpdate, "monthly"))
        )
      urlset = element "urlset" (DHS.singleton "xmlns" "http://www.sitemaps.org/schemas/sitemap/0.9")
      mkElm :: DT.Text -> DT.Text -> [Node]
      mkElm tag elm =
        element tag DHS.empty (text $ ltos elm)

      mkNode :: (DT.Text, DT.Text, DT.Text) -> [Node]
      mkNode (url, up, freq) =
        mkElm "loc" url
        ++ mkElm "lastmod" up
        ++ mkElm "changefreq" freq

      mkNodeList :: ToSitemap a => [a] -> [[Node]]
      mkNodeList =
        Prelude.map (\x -> element "url" DHS.empty (mkNode $ params x))
        where
          params ar = runReader (serialize ar) (config su)

      ltos :: DT.Text -> DTL.Text -- Xmlbfのバージョンが上がると不要になる
      ltos lt = DTL.pack (DT.unpack lt)

      lastUpdate :: DT.Text
      lastUpdate =
        if Prelude.null $ articles su
           then "2023-10-01" -- default date
           else maybe "" tz2text (EA.released_at $ DL.head $ articles su)
      topUrl = baseUrl . extConfig

