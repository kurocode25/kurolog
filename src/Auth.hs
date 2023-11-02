module Auth (authCheck)
where

import Servant
    ( BasicAuthResult(Unauthorized, Authorized, BadPassword),
      BasicAuthData(BasicAuthData),
      BasicAuthCheck(BasicAuthCheck) )
import Entity.User
import qualified Database.Entity.DBUser as EDU
import Database.PostgreSQL.Simple
import Database.Sql (getUserByCodeSql)
import Control.Monad (when)

import Repository (execQuery)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as DTI (putStrLn)
import Data.ByteString
import Utils (mkPassword)
import Mapper (toUser)
import Config as C

-- | Check function for BasicAuth
--
-- if 'Debug' mode, put out debug infomation.
authCheck :: Config -> BasicAuthCheck User
authCheck config =
  BasicAuthCheck check
   where
    check :: BasicAuthData -> IO (BasicAuthResult User)
    check (BasicAuthData username pass) = do
      musr <- getUser username
      case musr of
        Nothing -> return Unauthorized
        Just usr -> do
          when (isDebug config) $ do
            DTI.putStrLn $ mkPassword (decodeUtf8 pass, salt config)
            DTI.putStrLn $ EDU.password usr
            DTI.putStrLn $ decodeUtf8 username
            DTI.putStrLn $ EDU.name usr
          if decodeUtf8 username == EDU.code usr && mkPassword (decodeUtf8 pass, salt config) == EDU.password usr
            then return $ Authorized $ toUser usr
            else return BadPassword

    getUser :: ByteString -> IO (Maybe EDU.DBUser)
    getUser name = do
      li <- execQuery (pool config) getUserByCodeSql (Only $ decodeUtf8 name)
      if Prelude.null li
        then return Nothing
        else return $ Just $ Prelude.head li

    isDebug = C.isDebug . C.extConfig
    salt = C.passwordSalt . C.extConfig
    pool = C.pool

