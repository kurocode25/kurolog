{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Action (server) where

import Servant
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Database.Sql
import Database.Entity.Article as DEA
import Database.Entity.DBUser as DEDU 

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Entity.DetailArticle as ED ( DetailArticle(tags) )
import Entity.Article as A
import Entity.Tag
import Entity.RequestArticle
import Entity.Category
import Entity.ArticleList
import Entity.User as EU
import Entity.AuthRequest as EAUR
import Entity.Feeder
import Entity.Sitemap
import Entity.ResultResponse (ResultResponse(ResultResponse))

import Data.Time (getZonedTime)
import Data.Text(Text)
import Data.UUID.Types
import Data.Maybe (fromMaybe)

import Router(API, ServerM, getPool, getExtConfig)
import Utils (mkPassword, tz2text)
import View.ArticleView
import Repository
import Mapper
import Config

{- Action module defined API actions -}


-- Action for API request
server :: ServerT API ServerM
server = 
  atomFeed
  :<|> siteMap
  :<|> Repository.getTopPageView
  :<|> ssr
  :<|> Repository.getPageView
  :<|> articleGET
  :<|> detailArticleGET
  :<|> Repository.getCategoryList
  :<|> categoryGET
  :<|> tagListGET
  :<|> tagGET
  :<|> userLogin
  :<|> Repository.getPageList
  :<|> Repository.getPage
  :<|> protectedOP
  where
  articleGET :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ServerM ArticleList
  articleGET = Repository.getArticleList
  
  detailArticleGET :: Text -> Maybe Text -> ServerM DetailArticle
  detailArticleGET slug status = do
    p <- pool <$> ask
    list <- case status of
      Nothing -> execQuery p (getDetailArticleBySlugSql <> " order by a.released_at asc") (Only slug)
      Just st -> do
        let q = getDetailArticleBySlugSql <> " and a.status = ? order by a.released_at asc"
        execQuery p q (slug, st)
    if Prelude.null list
      then  throwError err404
      else do
        let articleId = DEA.id $ head list
        newtags <- execQuery p getTagsSqlByArticleId (Only articleId)
        return ((toDetailArticle $ head list) {ED.tags = map toTag newtags})

  categoryGET :: Text -> ServerM Category
  categoryGET = getCategory

  tagGET :: Text -> ServerM Tag
  tagGET slug = do
    list <- Repository.getTagListBySlug slug
    if Prelude.null list
      then throwError err404
      else return $ head list

  tagListGET :: ServerM [Tag]
  tagListGET = Repository.getTagList

  userLogin :: AuthRequest -> ServerM ResultResponse
  userLogin ar = do
    p <- pool <$> ask
    salt <- passwordSalt . extConfig <$> ask
    usr <- execQuery p getUserByCodeSql $ Only $ EAUR.username ar
    if mkPassword (EAUR.password ar, salt) == refPass usr
      then return $ ResultResponse True "Authentication is success!"
      else return $ ResultResponse False "username or password is bad"
    where
      refPass :: [DBUser] -> Text
      refPass u =
        DEDU.password $ head u

  atomFeed :: ServerM Feeder
  atomFeed = getFeedList

  siteMap :: ServerM Sitemap
  siteMap = getSiteMap

  ssr :: Text -> Text -> ServerM ArticleView
  ssr = getSSR
    
  protectedOP u =
    articlePOST u
    :<|> articlePUT u
    :<|> articleDELETE u
    :<|> categoryPOST u
    :<|> categoryPUT u
    :<|> categoryDelete u
    :<|> tagPOST u
    :<|> tagPUT u
    :<|> tagDelete u
    :<|> userGET u
    :<|> userListGET u
    :<|> userPOST u
    :<|> userPUT u
    :<|> Repository.createPage
    :<|> Repository.updatePage
    :<|> Repository.deletePage

    where

    articlePOST :: User -> RequestArticle -> ServerM NoContent
    articlePOST = Repository.createArticle

    articlePUT :: User ->  RequestArticle -> ServerM NoContent
    articlePUT usr = Repository.updateArticle

    articleDELETE :: User -> Text -> ServerM NoContent
    articleDELETE usr = Repository.deleteArticle
    
    categoryPOST :: User -> Category -> ServerM NoContent
    categoryPOST usr = Repository.createCategory
    
    categoryPUT :: User -> Category -> ServerM NoContent
    categoryPUT usr = Repository.updateCategory

    categoryDelete :: User -> Text -> ServerM NoContent
    categoryDelete usr = Repository.deleteCategory

    tagPOST :: User -> Tag -> ServerM NoContent
    tagPOST usr = Repository.createTag

    tagPUT :: User -> Tag -> ServerM NoContent
    tagPUT usr = Repository.updateTag

    tagDelete :: User -> Text -> ServerM NoContent
    tagDelete usr = Repository.deleteTag

    userListGET :: User -> ServerM [User]
    userListGET usr = getUserList
    userGET usr = getUserById
    userPOST usr = createUser
    userPUT usr = updateUser

getCategory :: Text -> ServerM Category
getCategory slug = do
  p <- pool <$> ask
  list <- execQuery p sql (Only slug)
  return $ toCategory $ head list
    where
      sql = getCategorySql False $ Just FSlug

createUser :: User -> ServerM ResultResponse
createUser  usr = do
  p <- pool <$> ask
  salt <- passwordSalt . extConfig <$> ask
  res <- execQuery p createUserSql (params salt)
  return $ ResultResponse True (toText $ fst $ head (res :: [(UUID, ZonedTimestamp)]))
  where
    params s = ( EU.code usr
               , EU.name usr
               , EU.email usr
               , mkPassword (fromMaybe "" (EU.password usr), s)
               )

updateUser :: User ->  ServerM ResultResponse
updateUser usr = do
  pool <- getPool
  salt <- passwordSalt . extConfig <$> ask
  case EU.id usr of
    Nothing -> throwError err400
    Just i -> do
      zt <- liftIO getZonedTime
      let  params = ( EU.code usr
                , EU.name usr
                , EU.email usr
                , mkPassword $ (fromMaybe "" (EU.password usr), salt)
                , zt
                , i
                )
      res <- execQuery pool updateUserByIdSql params
      return $ ResultResponse True (toText $ fst $ head (res :: [(UUID, ZonedTimestamp)]))

getUserById :: UUID -> ServerM User
getUserById i = do
  pool <- getPool
  res <- execQuery pool getUserByIdSql (Only i)
  if Prelude.null res
    then throwError err404
    else return $ toUser $ head res

getUserList :: ServerM [User]
getUserList  = do
  pool <- getPool
  res <- execQuery_ pool getUserListSql
  return $ map toUser res

getFeedList :: ServerM Feeder
getFeedList = do
  url <- baseUrl . extConfig <$> ask
  p <- pool <$> ask
  c <- ask
  res <- execQuery_ p q
  conf <- getExtConfig
  return $ Feeder (blogTitle conf) url (updated res) (as res) c
  where
    q = getArticlesSql <> (" where a.status = 200 order by a.released_at desc" :: Query)
    updated r =
      if Prelude.null $ as r
         then "2023-10-01" -- default date, if db has no articles, this value isn't used.
         else tz2text $ A.updated_at $ head $ as r
    as = map toArticle

getSiteMap :: ServerM Sitemap
getSiteMap = do
  p <- pool <$> ask
  c <- ask
  articles <- execQuery_ p q
  categories <- execQuery_ p categoryQuery
  pages <- execQuery_ p pageQ
  return $ Sitemap (map toArticle articles) (map toCategory categories) (map toPage pages) c
  where
    q = getArticlesSql <> (" where a.status = 200 order by a.released_at desc" :: Query)
    pageQ = getPageListSql $ Just True
    categoryQuery = getCategorySql False Nothing
    
getSSR :: Text -> Text -> ServerM ArticleView
getSSR dirName slug = do
  p <- pool <$> ask
  r <- execQuery p q param
  c <- ask
  let dir = articleDir $ extConfig c
  if dirName /= fromMaybe "posts" dir
     then throwError err404
     else do
      if Prelude.null r
        then  throwError err404
        else return $ ArticleView (toDetailArticle $ head r) c

  where
    q = getDetailArticleBySlugWithStatusSql
    param = (slug :: Text, In ["200"] :: In [Text])

