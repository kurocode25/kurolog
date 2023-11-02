{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Entity.Feeder
where

import Entity.Article as Article
import Servant.AtomFeed
import qualified Text.Atom.Feed as Atom
import Data.UUID.Types (toText)
import Data.Text (Text, pack)
import Config
import Data.Maybe

data Feeder = Feeder
  { title :: Text
  , uuid :: Text
  , updated :: Text
  , articles :: [Article]
  , config :: Config
  } deriving (Show)

instance ToFeed Feeder where
  toFeed :: Feeder -> Atom.Feed
  toFeed (Feeder t u up a c) = feed {Atom.feedEntries = fmap toEntry a}
    where
      feed = Atom.nullFeed u (Atom.TextString t) up
      toEntry :: Article -> Atom.Entry
      toEntry ar =
        (Atom.nullEntry 
          (toText $ Article.id ar)
          (Atom.TextString $ Article.title ar)
          (pack $ show $ Article.updated_at ar))
        {Atom.entryLinks = [Atom.nullLink (base <> "/" <> dir <> "/" <> Article.slug ar)]}
      base = baseUrl $ extConfig c
      dir = fromMaybe "posts" $ articleDir $ extConfig c
        
  
