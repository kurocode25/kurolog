{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Servant.HTML.Lucid2
where

import Servant ( Proxy, Accept(contentType), MimeRender(..) )
import Network.HTTP.Media ( MediaType, (//), (/:) )
import Data.ByteString.Lazy ( ByteString )
import Lucid (ToHtml (..), renderBS)

data HTML

instance Servant.Accept HTML where
  contentType :: Proxy HTML -> MediaType 
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTML a where
  mimeRender :: Proxy HTML -> a -> ByteString
  mimeRender _ = renderBS . toHtml


