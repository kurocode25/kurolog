{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config
( Config(..)
, ExtConfig(..)
, setCorsOrigins
, setConnectInfo
, myCorsResourcePolicy
, CorsOrigins
, defaultConfFilePath
) where

import Database.PostgreSQL.Simple
    ( defaultConnectInfo, Connection,
      ConnectInfo(connectDatabase, connectHost, connectUser, connectPort,
                  connectPassword) )
import Data.Text as T
import Data.Text.Encoding as T
import Data.ByteString
import Data.Pool ( Pool )
import qualified Data.List as L
import Network.Wai.Middleware.Cors as WMC

import qualified Network.HTTP.Types as HTTP

import Dhall
import Data.Word

data Config = Config
  { extConfig :: ExtConfig
  , pool :: Pool Connection
  } deriving (Show)

data ExtConfig = ExtConfig
  { baseUrl      :: Text
  , port         :: Int
  , origins      :: [Text]
  , pageLimit    :: Int
  , blogTitle    :: Text
  , description  :: Text
  , articleDir   :: Maybe Text
  , mode         :: Text
  , passwordSalt :: Text
  , isDebug      :: Bool 
  , ogpImage     :: Text
  , ogpImageForX :: Maybe Text
  , accountForX  :: Maybe Text
  , locale       :: Text
  , dbInfo       :: DBInfo
  } deriving (Generic, Show)

data DBInfo = DBInfo
  { dbHost       :: String
  , dbUser       :: String
  , dbName       :: String
  , dbPass       :: String
  , dbPort       :: Word16
  } deriving (Generic, Show)

instance FromDhall ExtConfig
instance FromDhall DBInfo

setCorsOrigins :: Text -> [Text] -> Maybe ([ByteString], Bool)
setCorsOrigins m c = do
  case m of
    "development" -> Nothing
    "production" -> do
      if L.null c
         then Nothing
         else Just (Prelude.map T.encodeUtf8 c, False)
    _ -> Nothing

setConnectInfo :: DBInfo -> ConnectInfo
setConnectInfo dbinfo =
  defaultConnectInfo
    { connectHost = dbHost dbinfo
    , connectUser = dbUser dbinfo
    , connectPort = dbPort dbinfo
    , connectPassword = dbPass dbinfo
    , connectDatabase = dbName dbinfo
    }

defaultConfFilePath :: String
defaultConfFilePath = "/etc/kurolog/conf.dhall"

type CorsOrigins = Maybe ([Origin], Bool)

myCorsResourcePolicy :: CorsResourcePolicy
myCorsResourcePolicy = simpleCorsResourcePolicy
  { corsMethods = methods
  , corsRequestHeaders = headers
  }
  where
    methods :: [HTTP.Method]
    methods = [ "GET"
              , "HEAD"
              , "POST"
              , "PUT"
              , "DELETE"
              ]
    headers :: [HTTP.HeaderName]
    headers =
      [ "Content-Type"
      , "Authorization"
      , "Accept-Encoding"
      ]
