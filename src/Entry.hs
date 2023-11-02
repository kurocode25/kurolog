-- Copyright © 2023 Kuro_CODE25
-- Licensed under the MIT License.
-- See https://opensource.org/licenses/MIT for details.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Entry
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Cors as WMC

import Control.Monad (when)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Router(API, ServerM)

import Database.PostgreSQL.Simple ( close, connect )

import Data.Pool ( createPool )
import Data.Text as T

import qualified Dhall as D
import Auth (authCheck)
import Config
import Servant
import Action

startApp :: String -> IO ()
startApp confPath = do
  ex <- D.input D.auto $ T.pack confPath
  p <- createPool (connect $ setConnectInfo $ dbInfo ex) close 2 10 10
  let conf = Config { extConfig = ex, pool = p } 
  when (isDebug ex) $ do
    putStrLn ("Start Blog API! Connect to: " ++ show (port ex))
    print conf
  flip runReaderT conf $ do
    c <- ask
    app >>= liftIO . run (po c)
      where
        po = port . extConfig

type AppM = ReaderT Config IO

nt :: Config -> ServerM a -> Handler a
nt c x = runReaderT x c

app :: AppM Application
app = do
  c <- ask
  let ctx = authCheck c :. EmptyContext
  let ex = extConfig c
  let co = setCorsOrigins (mode ex) (origins ex)
  return $ policy co $ serveWithContextT api ctx (nt c) server

api :: Proxy API
api = Proxy

policy :: CorsOrigins -> Middleware
policy o = cors $ const $ Just myCorsResourcePolicy {corsOrigins = o}

