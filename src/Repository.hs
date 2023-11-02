{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Repository where

import Entity.Category as C
import Entity.Tag as T
import Entity.ArticleList as AL
import Entity.RequestArticle as ER
import Entity.Page as EP
import Entity.User as EU

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow()
import Database.Entity.Category as DEC
import Database.Sql

import Data.Pool (Pool, withResource)
import Data.Maybe (fromMaybe)
import Data.UUID.Types
import Data.Text(Text)
import Data.Time.LocalTime

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Config

import Servant
import Mapper (toPage, toTag, toCategory, toArticleList, toArticle)
import Router (ServerM, getPool)
import View.TopPageView
import View.PageView

{-
  Repository module provides connecting database functions
-}

getCategoryList :: ServerM [Category]
getCategoryList = do
  p <- getPool
  list <- liftIO $ withResource p (`query_` sql)
  return $ map toCategory list
    where
      sql = getCategorySql True Nothing

getTagList :: ServerM [Tag]
getTagList = do
  p <- pool <$> ask
  list <- liftIO $ withResource p (`query_` getTagListSql)
  return $ map toTag list

getTagListBySlug :: Text -> ServerM [Tag]
getTagListBySlug slug = do
  pool <- getPool
  list <- execQuery pool getTagBySlugSql (Only slug)
  return $ map toTag list

createArticle :: User -> RequestArticle -> ServerM NoContent
createArticle user na = do
  pool <- getPool
  res <- execQuery pool insertArticle mkParams
  _ <- if Prelude.null (ER.tags na)
  then 
    liftIO $ return 0 -- dont regist tags
  else
    liftIO $ withResource pool
      (\conn -> executeMany conn createTagsRelationSql (mkTagParams (unpackRes res) (ER.tags na)))
  return NoContent
  where
    mkParams =
      ( ER.slug na
      , ER.status na
      , EU.id user
      , ER.category_id na
      , ER.title na
      , ER.image na
      , ER.markdown na
      )
    unpackRes :: [(UUID, ZonedTime)] -> UUID
    unpackRes = fst . head
    mkTagParams :: UUID -> [UUID] -> [(UUID, UUID)]
    mkTagParams a = map (\x -> (a, x))
    
updateArticle :: RequestArticle -> ServerM NoContent
updateArticle req = do
  pool <- getPool
  case ER.id req of
    Nothing -> return NoContent
    Just articleId -> do
      tz <- liftIO getZonedTime
      -- operationの導入ではなく、DBとrequestのstatusの比較の方が良いか要検討。
      _ <- if ER.operation req == 300
        then do -- release operation
          let params =
                ( ER.slug req
                , ER.status req
                , ER.category_id req
                , ER.title req
                , ER.image req
                , ER.markdown req
                , tz
                , fromMaybe tz (ER.released_at req)
                , articleId
                )
          let qu = updateArticleSql <> ", released_at=? where id=?"
          liftIO $ withResource pool (\conn -> execute conn qu params) >>
            -- last_postを更新する処理
            execQ pool updateLastPostSql (tz, articleId)
        else do -- nomarl update operation
          let params =
                ( ER.slug req
                , ER.status req
                , ER.category_id req
                , ER.title req
                , ER.image req
                , ER.markdown req
                , tz
                , ER.released_at req
                , articleId
                )
          let qu = updateArticleSql <> ", released_at=? where id=?"
          liftIO $ withResource pool (\conn -> execute conn qu params)

      -- Update Tags list
      if Prelude.null (ER.tags req)
      then do
        _ <- liftIO $ withResource pool (\conn -> execute conn deleteTagsRelationSql (Only articleId))
        return NoContent
      else do
        _ <- liftIO $ withResource pool (\conn -> execute conn deleteTagsRelationSql (Only articleId))
        _ <- liftIO $ withResource pool 
          (\conn -> executeMany conn createTagsRelationSql (mkparam articleId (ER.tags req)))
        return NoContent
    where
      mkparam :: UUID -> [UUID] -> [(UUID, UUID)]
      mkparam i = map (\item -> (i, item))

createCategory :: Category -> ServerM NoContent
createCategory ca = do
  pool <- getPool
  liftIO $ withResource pool (\conn -> execute conn insertCategorySql params) >>
    return NoContent
  where
    params = ( C.slug ca
             , C.name ca
             )

updateCategory :: Category -> ServerM NoContent
updateCategory ca = do
  pool <- getPool
  now <- liftIO getZonedTime
  let params = ( C.slug ca, C.name ca, now, C.id ca)
  execQ pool updateCategorySql params >> return NoContent
  -- liftIO $ withResource pool (\conn -> execute conn updateCategorySql params) >>
  --   return NoContent

createTag :: Tag -> ServerM NoContent
createTag ca = do
  pool <- getPool
  liftIO $ withResource pool (\conn -> execute conn insertTagSql params) >>
    return NoContent
  where
    params = ( T.slug ca
             , T.name ca
             )

updateTag :: Tag -> ServerM NoContent
updateTag ca = do
  pool <- getPool
  liftIO $ withResource pool (\conn -> execute conn updateTagSql params) >>
    return NoContent
  where
    params = ( T.slug ca
             , T.name ca
             , T.id ca
             )

getArticleList :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ServerM ArticleList
getArticleList cq tq sq pq = do
  p <- getPool
  limit <- pageLimit . extConfig <$> ask
  itemMax <- getArticleTotal cq tq sq
  offsetAndLimit <- case pq of
    Nothing -> return (0, 1)
    Just num -> return (limit * num - limit, num)
  let offset = fst offsetAndLimit
  let page = snd offsetAndLimit
  list <- case tq of
    Nothing -> do
      case cq of
        Nothing -> do
          case sq of
            Nothing -> do
              let qu = getArticlesSql <> " order by A.released_at desc limit ? offset ?"
              execQuery p qu (limit, offset)
            Just s -> do 
              let qu = getArticlesSql <> " where A.status = ? order by A.released_at desc limit ? offset ?"
              execQuery p qu (s, limit, offset)
        Just c -> do
          case sq of
            Nothing -> do
              let qu = getArticlesSql <> " where C.slug=? order by A.released_at desc limit ? offset ?"
              execQuery p qu (c,limit, offset)
            Just s -> do
              let qu = getArticlesSql <> " where A.status = ? and C.slug = ? order by A.released_at desc limit ? offset ?"
              execQuery p qu (s, c ,limit, offset)
    Just t ->
      case cq of
        Nothing ->
          case sq of
            Nothing -> do
              execQuery p getArticlesByTagQuerySql (Only t)
            Just s -> do
              let qu = getArticlesByTagQuerySql <> " and A.status=? order by A.released_at desc limit ? offset ?"
              execQuery p qu (t, s, limit, offset)
        Just c ->
          case sq of
            Nothing -> do
              let qu = getArticlesByTagQuerySql <> " and C.slug=? order by A.released_at desc limit ? offset ?"
              execQuery p qu (t, c, limit, offset)
            Just s -> do
              let qu = getArticlesByTagQuerySql <> " and A.status=? and C.slug=? order by A.released_at desc limit ? offset ?"
              execQuery p qu (t, s, c, limit, offset)
  return $ toArticleList list page limit itemMax

getArticleTotal :: Maybe Text -> Maybe Text -> Maybe Text -> ServerM Int
getArticleTotal cq tq sq = do
  pool <- getPool
  list <- case tq of
    Nothing -> do
      case cq of
        Nothing -> do
          case sq of
            Nothing -> do
              let qu = getTotalArticleCountSql
              execQuery_ pool qu
            Just s -> do 
              let qu = getTotalArticleCountSql <> " where A.status = ?"
              execQuery pool qu (Only s)
        Just c -> do
          case sq of
            Nothing -> do
              let qu = getTotalArticleCountSql <> " where C.slug=?"
              execQuery pool qu (Only c)
            Just s -> do
              let qu = getTotalArticleCountSql <> " where A.status = ? and C.slug = ?"
              execQuery pool qu (s, c)
    Just t ->
      case cq of
        Nothing ->
          case sq of
            Nothing -> do
              execQuery pool getTotalAritcleCountByTagSql (Only t)
            Just s -> do
              let qu = getTotalAritcleCountByTagSql <> " and A.status=?"
              execQuery pool qu (t, s)
        Just c ->
          case sq of
            Nothing -> do
              let qu = getTotalAritcleCountByTagSql <> " and C.slug=?"
              execQuery pool qu (t, c)
            Just s -> do
              let qu = getTotalAritcleCountByTagSql <> " and A.status=? and C.slug=?"
              execQuery pool qu (t, s, c)
  if Prelude.null list
    then return 0
    else return $ fromOnly $ head list

deleteArticle :: Text -> ServerM NoContent
deleteArticle articleId = do
  pool <- getPool
  _ <- liftIO $ withResource pool (\conn -> execute conn q p)
  return NoContent
    where
      q = deleteArticleSql FId
      p = Only articleId

-- | Delete Category
-- |
-- | category ID :: Text
deleteCategory :: Text -> ServerM NoContent
deleteCategory catId = do
  pool <- getPool
  res <- execQuery pool q (Only catId)
  if DEC.count (head res) /= 0  -- Don't delete category that has articles
     then throwError err409
     else do
       liftIO $ withResource pool (\conn -> execute conn dq (Only catId)) >> return NoContent
  where
    q = getCategorySql False $ Just FId
    dq = deleteCategorySql FId

-- | Delete Tag Function
--
-- delete tag by ID
deleteTag :: Text -> ServerM NoContent
deleteTag tagId = do
  pool <- getPool
  liftIO $ withResource pool (\conn -> execute conn q param) >> return NoContent
  where
    q = deleteTagSql
    param = Only tagId

-- | Create Page
createPage :: Page -> ServerM NoContent
createPage page = do
  pool <- getPool
  execQ pool insertPageSql params >> return NoContent
  where
    params = ( EP.slug page
             , EP.title page
             , EP.markdown page
             , EP.pub_flag page
             )

-- | update Page
updatePage :: Page -> ServerM NoContent
updatePage page = do
  pool <- getPool
  let slug = EP.slug page
  tz <- liftIO getZonedTime
  execQ pool updatePageSql (params slug tz) >> return NoContent
    where
    params sl tz =
      ( EP.slug page
      , EP.title page
      , EP.markdown page
      , tz
      , EP.pub_flag page
      , sl
      )

-- | delete Page by ID
deletePage :: Text -> ServerM NoContent
deletePage pageId = do
  pool <- getPool
  execQ pool deletePageSql (Only pageId) >> return NoContent

-- | get PageList
getPageList :: Maybe Bool -> ServerM [Page]
getPageList st = do
  pool <- getPool
  list <- execQuery_ pool (getPageListSql st)
  return $ map toPage list

getPageView :: Text -> Maybe Bool -> ServerM PageView
getPageView slug flag = do
  c <- ask
  pool <- getPool
  list <- execQuery pool (getPageSql flag) (Only slug)
  if Prelude.null list
     then throwError err404
     else return $ PageView (Mapper.toPage (head list)) c

getPage :: Text -> Maybe Bool -> ServerM Page
getPage slug flag = do
  pool <- getPool
  list <- execQuery pool (getPageSql flag) (Only slug)
  if Prelude.null list
     then throwError err404
     else return $ Mapper.toPage (head list)

getTopPageView :: ServerM TopPageView
getTopPageView = do
  c <- ask
  pool <- getPool
  list <- execQuery_ pool q
  return $ TopPageView (map toArticle list) c
    where
      q = baseGetArticleQuery <> " where a.status = 200"
  
-- クエリの置換をするバージョン
execQuery :: (MonadIO m, ToRow q, FromRow r) =>
  Pool Connection -> Query -> q -> m [r]
execQuery pool q p = liftIO $ withResource pool (\conn -> query conn q p)

-- クエリの置換をしないバージョン
execQuery_ :: (MonadIO m, FromRow r) => Pool Connection -> Query -> m [r]
execQuery_ pool q = liftIO $ withResource pool (`query_` q)

execQ pool q p = liftIO $ withResource pool (\conn -> execute conn q p)
execQ_ pool q =  liftIO $ withResource pool (`execute_` q)
