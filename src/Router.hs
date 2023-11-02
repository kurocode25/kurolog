{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Router where

import Servant
import Servant.AtomFeed
import Servant.XML
import Servant.HTML.Lucid2

import Entity.Category
import Entity.DetailArticle (DetailArticle)
import Entity.ArticleList
import Entity.RequestArticle
import Entity.Tag
import Entity.User (User)
import Entity.ResultResponse
import Entity.AuthRequest (AuthRequest)
import Entity.Feeder
import Entity.Sitemap
import Entity.Page

import View.TopPageView
import View.ArticleView
import View.PageView

import Config
import Control.Monad.Trans.Reader

import Data.Text
import Data.UUID.Types
import Data.Pool ( Pool )
import Database.PostgreSQL.Simple (Connection)


type ServerM = ReaderT Config Handler

getPool :: ServerM (Pool Connection)
getPool = pool <$> ask

getExtConfig :: ServerM ExtConfig
getExtConfig = extConfig <$> ask

-- | API settings
type API =
    -- Atom Feeder
    "feed.atom" :> Get '[ATOM] Feeder
    -- sitemap
    :<|> "sitemap.xml" :> Get '[XML] Sitemap
    -- SSR for OGP
    :<|> "ssr" :> Get '[HTML] TopPageView
    :<|> "ssr" :> Capture "dirName" Text :> Capture "slug" Text :> Get '[HTML] ArticleView
    :<|> "ssr" :> Capture "slug" Text :> QueryParam "pub" Bool :> Get '[HTML] PageView
    -- API for blog
    :<|> "blog" :> "v1" :>
    (
      -- Public API
      "article" :> QueryParam "category" Text :> QueryParam "tag" Text :>
        QueryParam "status" Text :> QueryParam "p" Int :> Get '[JSON] ArticleList
      :<|> "article" :> Capture "slug" Text :> QueryParam "status" Text :> Get '[JSON] DetailArticle
      :<|> "category" :> Get '[JSON] [Category]
      :<|> "category" :> Capture "slug" Text :> Get '[JSON] Category
      :<|> "tag" :> Get '[JSON] [Tag]
      :<|> "tag" :> Capture "slug" Text :> Get '[JSON] Tag
      :<|> "user" :> "login" :> ReqBody '[JSON] AuthRequest :> Post '[JSON] ResultResponse
      :<|> "page" :> QueryParam "pub" Bool :> Get '[JSON] [Page]
      :<|> "page" :> Capture "slug" Text :> QueryParam "pub" Bool :> Get '[JSON] Page
      -- Protected API
      :<|> BasicAuth "KuroDigitalLog" User :>
      ( 
        "article" :> ReqBody '[JSON] RequestArticle :> Post '[JSON] NoContent
        :<|> "article" :> ReqBody '[JSON] RequestArticle :> Put '[JSON] NoContent
        :<|> "article" :> Capture "id" Text :> Delete '[JSON] NoContent
        :<|> "category" :> ReqBody '[JSON] Category :> Post '[JSON] NoContent
        :<|> "category" :> ReqBody '[JSON] Category :> Put '[JSON] NoContent
        :<|> "category" :> Capture "id" Text :> Delete '[JSON] NoContent
        :<|> "tag" :> ReqBody '[JSON] Tag :> Post '[JSON] NoContent
        :<|> "tag" :> ReqBody '[JSON] Tag :> Put '[JSON] NoContent
        :<|> "tag" :> Capture "id" Text :> Delete '[JSON] NoContent
        :<|> "user" :> Capture "id" UUID :> Get '[JSON] User
        :<|> "user" :> Get '[JSON] [User]
        :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] ResultResponse
        :<|> "user" :> ReqBody '[JSON] User :> Put '[JSON] ResultResponse
        :<|> "page" :> ReqBody '[JSON] Page :> Post '[JSON] NoContent
        :<|> "page" :> ReqBody '[JSON] Page :> Put '[JSON] NoContent
        :<|> "page" :> Capture "id" Text :> Delete '[JSON] NoContent
      )
    )
