{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Servant.AtomFeed (ATOM, ToFeed, toFeed)
where
import Servant ( Proxy, Accept(contentType), MimeRender(..) )
import Network.HTTP.Media ( MediaType, (//), (/:) )
import Text.Atom.Feed (Feed)
import Text.Atom.Feed.Export (textFeed)
import Text.Feed.Export (textFeedWith)
import qualified Text.Feed.Types as TF (Feed(..))
import Text.XML (def, rsPretty)
import Data.ByteString.Lazy ( ByteString )
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Maybe (fromJust)

data ATOM

instance Servant.Accept ATOM where
  contentType :: Proxy ATOM -> MediaType
  contentType _ = "application" // "atom+xml" /: ("charset", "utf-8")

instance ToFeed a => MimeRender ATOM a where
  mimeRender :: Proxy ATOM -> a -> ByteString
  mimeRender _ x = encodeUtf8 $ fromJust (textFeedWith def{rsPretty = True} (TF.AtomFeed $ toFeed x))

class ToFeed a where
  toFeed :: a -> Feed
