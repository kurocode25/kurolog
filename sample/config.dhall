{- 
  Kurolog Configuration file  
  This file is written in Dhall language. For any questions regarding the Dhall language, please visit
  https://dhall-lang.org/

  This is sample file. Please change values.
-}

-- Basic Settings
let baseUrl      : Text      = "http://localhost" -- Blog API base URL
let port         : Integer   = +3000 -- Port which Blog API run on

-- origins
-- This value is Origin's list that used CORS setting.
-- example:
-- [ "http://hoge.example.com", "https://huga.example.com" ] or [] : List Text
let origins      : List Text = [] : List Text

-- pageLimit
-- pageLmit param for limit of pagination
let pageLimit    : Integer   = +30

-- passwordSalt
-- This is Salt value for encrypt password.
let passwordSalt : Text      = "passwordsalt" -- CAUTION!: this is sample value. Please change value.

-- articleDir
-- If this value is set to "archives", the URL of each article will be set like "http://example.com/archives/hogehoge". 
-- If this value is not set, the initial value is "posts".
let articleDir   : Optional Text = Some "posts" 

-- blogTitle
let blogTitle    : Text      = "BLOG TITLE"

-- description
let description  : Text      = "description of blog"

-- ogp infomation
let ogpImage     : Text      = "http://localhost/media/img/ogp_img.png" -- OGP image URL
let ogpImageForX : Optional Text = Some "http://localhost/media/img/ogp_x.png" -- OGP image URL for X(Twitter)
let accountForX  : Optional Text = Some "@x_account_sample" -- set X(Twitter) account name
let locale       : Text      = "ja_JP" -- set locale code. example: en_US

-- Build mode
let mode         : Text      = "development" -- development | production

-- Debug mode
let isDebug      : Bool      = True  -- True | False

-- PostgreSQL connect settings
let DBInfo = { dbHost : Text , dbUser : Text , dbName : Text , dbPass : Text, dbPort : Natural }
let dbInfo : DBInfo =
  { dbHost = "localhost" -- PostgreSQL host name
  , dbUser = "username"  -- PostgreSQL User name
  , dbName = "database"  -- PostgreSQL Database name
  , dbPass = "password"  -- PostgreSQL password
  , dbPort = 5432        -- PostgreSQL connection port
  }

in { baseUrl
   , origins
   , port
   , mode
   , isDebug
   , pageLimit
   , blogTitle
   , description
   , dbInfo
   , ogpImage
   , ogpImageForX
   , accountForX
   , passwordSalt
   , locale
   , articleDir
   }

