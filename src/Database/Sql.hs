{-# LANGUAGE OverloadedStrings #-}
module Database.Sql where

import Database.PostgreSQL.Simple
import qualified Database.Entity.Article as A

data QFlag = FId | FSlug
data Status = All | Public | UnPublic

{- SQL for postgreSQL -}

-- 記事情報取得のベースSQL

getTotalArticleCountSql :: Query
getTotalArticleCountSql =
  "select \
  \count(*) \
  \from articles as A inner join categories as C on \
  \A.category_id = C.id"

getTotalAritcleCountByTagSql :: Query
getTotalAritcleCountByTagSql = 
  "select \
  \count(*) \
  \from articles as A inner join categories as C \
  \on a.category_id = C.id \
  \inner join articles_tags as AT on A.id = AT.article_id \
  \inner join tags as T on at.tag_id = T.id \
  \where T.slug = ?" 

baseGetArticleQuery :: Query
baseGetArticleQuery = 
  "select \
  \A.id \
  \, A.slug\
  \, A.title\
  \, A.released_at\
  \, A.created_at\
  \, A.updated_at\
  \, A.status\
  \, A.image\
  \, A.markdown\
  \, A.category_id\
  \, C.slug\
  \, C.name \
  \, C.created_at \
  \, C.updated_at \
  \, C.last_posted_at \
  \from articles as A inner join categories as C on \
  \A.category_id = C.id"

getArticlesByTagQuerySql ::  Query
getArticlesByTagQuerySql =
  "select \
  \A.id \
  \, A.slug\
  \, A.title\
  \, A.released_at\
  \, A.created_at\
  \, A.updated_at\
  \, A.status\
  \, A.image\
  \, A.markdown\
  \, A.category_id\
  \, C.slug\
  \, C.name \
  \, C.created_at \
  \, C.updated_at \
  \, C.last_posted_at \
  \from articles as A inner join categories as C \
  \on a.category_id = C.id \
  \inner join articles_tags as AT on A.id = AT.article_id \
  \inner join tags as T on at.tag_id = T.id \
  \where T.slug = ?" 

-- Category Listを取得
getArticlesSql :: Query
getArticlesSql = baseGetArticleQuery

-- IDを指定して記事情報を取得
getDetailArticleByIdSql :: Query
getDetailArticleByIdSql = baseGetArticleQuery <>
 " where A.id = ? order by A.released_at asc"

-- slugを指定して記事情報を取得
getDetailArticleBySlugSql :: Query
getDetailArticleBySlugSql = baseGetArticleQuery <>
 " where A.slug = ?"

-- slugとstatusを指定して記事情報を取得
getDetailArticleBySlugWithStatusSql :: Query
getDetailArticleBySlugWithStatusSql = baseGetArticleQuery <>
 " where A.slug = ? and A.status in ? order by A.released_at asc"

type IsAll = Bool
type HasArticle = Bool

-- | カテゴリー取得SQL
-- isAll :: Bool
-- 
-- flag :: Maybe QFlag
getCategorySql :: IsAll -> Maybe QFlag -> Query
getCategorySql isAll flag =
  if isAll
     then
       base <> g
     else
       case flag of
         Nothing -> base <> g <> " having count(a.id) > 0"
         Just FId -> base <> " where c.id = ?" <> g
         Just FSlug -> base <> " where c.slug = ?" <> g
  where
    base :: Query
    base =
      "select \
      \c.id, c.slug, c.name, c.created_at, c.updated_at, c.last_posted_at, count(a.id) as count \
      \from \
      \categories as c left join articles as a on c.id = a.category_id"
    g :: Query
    g = " group by c.id"

-- タグ一覧を取得
getTagListSql :: Query
getTagListSql =
  "select id, slug, name from tags"

getTagBySlugSql :: Query
getTagBySlugSql =
  "select id, slug, name from tags where slug = ?"

getTagByIdSql :: Query
getTagByIdSql =
  "select id, slug, name from tags where id = ?"

-- article_idからタグ一覧を取得
getTagsSqlByArticleId :: Query
getTagsSqlByArticleId = 
  "select \
  \T.id, T.slug, T.name \
  \from articles_tags as A inner join tags as T on A.tag_id = T.id \
  \where A.article_id = ?"

insertArticle :: Query
insertArticle =
  "insert into articles \
  \(slug\
  \, status\
  \, user_id\
  \, category_id\
  \, title\
  \, image\
  \, markdown\
  \) values (?, ?, ?, ?, ?, ?, ?) \
  \returning id, created_at"

updateArticleSql :: Query
updateArticleSql =
  "update articles set \
  \slug = ? \
  \, status = ? \
  \, category_id = ? \
  \, title = ? \
  \, image = ? \
  \, markdown = ? \
  \, updated_at = ?"

-- 記事を削除
deleteArticleSql :: QFlag -> Query
deleteArticleSql FId = base <> " where id = ?"
  where
  base = "delete from articles"
deleteArticleSql FSlug = base <> " where slug = ?"
  where
  base = "delete from articles"

deleteTagsRelationSql :: Query
deleteTagsRelationSql =
  "delete from articles_tags where article_id = ?"

createTagsRelationSql :: Query
createTagsRelationSql =
  "insert into articles_tags \
  \(article_id\
  \, tag_id\
  \) values (?, ?)"

insertCategorySql :: Query
insertCategorySql =
  "insert into categories (slug, name) values (?,?)"

updateCategorySql :: Query
updateCategorySql =
  "update categories set slug = ?, name = ?, updated_at = ? \
  \where id = ?"

updateLastPostSql :: Query
updateLastPostSql =
  "update categories set last_post = ? where id = ?"

deleteCategorySql :: QFlag -> Query
deleteCategorySql FSlug =
  "delete from categories where slug = ?"
deleteCategorySql FId =
  "delete from categories where id = ?"

insertTagSql :: Query
insertTagSql =
  "insert into tags (slug, name) values (?,?)"

updateTagSql :: Query
updateTagSql =
  "update tags set slug = ?, name = ?, updated_at = now() \
  \where id = ?"

-- | Delete Tags SQL
deleteTagSql :: Query
deleteTagSql = "delete from tags where id = ?"

getUserListSql :: Query
getUserListSql =
  "select id, code, name, email, password from users"

getUserByCodeSql :: Query
getUserByCodeSql =
  "select id, code, name, email, password from users where code = ?"

getUserByIdSql :: Query
getUserByIdSql =
  "select id, code, name, email, password from users where id = ?"

createUserSql :: Query
createUserSql =
  "insert into users (code, name, email, password) values (?,?,?,?) \
  \returning id, created_at"

updateUserByIdSql :: Query
updateUserByIdSql =
  "update users set code = ?, name = ?, email = ?, password = ?, updated_at = ? \
  \where id = ? \
  \returning id, updated_at"

updateUserByCodeSql :: Query
updateUserByCodeSql =
  "update users set code = ?, name = ?, email = ?, password = ?, updated_at = ? \
  \where code = ? \
  \returning id, updated_at"

-- | SQL for get pages list
getPageListSql :: Maybe Bool -> Query
getPageListSql status =
  case status of
    Nothing    -> base
    Just True  -> base <> " where pub_flag = true"
    Just False -> base <> " where pub_flag = false"
  where
  base = 
    "select id, slug, title, markdown, created_at, updated_at, pub_flag from pages"

-- | get page SQL by slug
getPageSql :: Maybe Bool -> Query
getPageSql flag =
  case flag of
    Nothing -> base
    Just True -> base <> " and pub_flag = true"
    Just False -> base <> " and pub_flag = false"
  where
    base =
      "select id, slug, title, markdown, created_at, updated_at, pub_flag \
      \from pages where slug = ?"

-- | update page by slug 
updatePageSql :: Query
updatePageSql =
  "update pages set slug = ?, title = ?, markdown = ?, updated_at = ?, pub_flag = ? \
  \where slug = ?"

-- | delete page by ID 
deletePageSql :: Query
deletePageSql =
  "delete from pages where id = ?"

-- | create page
insertPageSql :: Query
insertPageSql =
  "insert into pages (slug, title, markdown, pub_flag) values (?,?,?,?)"
