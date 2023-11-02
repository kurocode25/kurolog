module Command.Migrate
  ( execMigrate
  , initDB
  ) where

import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple (connect, close)
import Dhall as D
import Data.Text as T
import Config


execMigrate :: FilePath -> FilePath -> IO ()
execMigrate dirPath confPath = do
  ext <- D.input D.auto $ T.pack confPath
  conn <- connect $ setConnectInfo $ dbInfo ext
  res <- runMigration
    conn 
    defaultOptions
    (MigrationDirectory dirPath)
  case res of
    MigrationError err -> putStrLn err
    MigrationSuccess -> putStrLn "migration is succeeded!!"
  close conn

initDB :: String -> IO ()
initDB confPath = do
  ext <- D.input D.auto $ T.pack confPath
  conn <- connect $ setConnectInfo $ dbInfo ext
  initRes <- runMigration
    conn
    defaultOptions
    MigrationInitialization
  case initRes of
    MigrationError err -> putStrLn err
    MigrationSuccess -> putStrLn "initialize is succeeded!!"
  close conn



