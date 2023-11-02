-- Copyright © 2023 Kohei Hatakeyama
-- Licensed under the MIT License.
-- See https://opensource.org/licenses/MIT for details.

module Main (main) where

import Entry
import System.Environment
import Command.MakeUser (mkUser)
import Command.Migrate (execMigrate, initDB)
import Command.HashPass (hashPass)
import Config

main :: IO ()
main = do
  args <- getArgs
  parse args
  where
    parse ["createuser"] = mkUser defaultConfFilePath
    parse ["createuser", confPath] = mkUser confPath
    parse ["serve"] = startApp defaultConfFilePath
    parse ["serve", confPath] = startApp confPath
    parse ["migrate", path] = execMigrate path defaultConfFilePath
    parse ["migrate", path, confPath] = execMigrate path confPath
    parse ["initdb", confPath] = initDB confPath
    parse ["initdb"] = initDB defaultConfFilePath
    parse ["hashpasswd", pass, confPath] = hashPass confPath pass
    parse ["hashpasswd", pass] = hashPass defaultConfFilePath pass
    parse ["--help"] = usage
    parse _ = usage

    usage :: IO ()
    usage = do
      putStrLn 
        "Usage: kurolog COMMAND [ARGS]\n\
        \       kurolog --help\n\n\
        \Commands:\n\
        \  createuser [CONFIG_FILE_PATH]            create blog user\n\
        \  serve [CONFIG_FILE_PATH]                 start server\n\
        \  migrate DIR_PATH [CONFIG_FILE_PATH]      migrate database\n\
        \  initdb [CONFIG_FILE_PATH]                initialize database\n\
        \  hashpasswd PASSWORD [CONFIG_FILE_PATH]   show hashed password\n"
