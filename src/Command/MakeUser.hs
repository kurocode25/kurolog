module Command.MakeUser (mkUser)
where
import Database.PostgreSQL.Simple as PS (connect, query, Connection)
import Database.Sql (createUserSql)
import Utils (mkPassword, Salt)
import Data.UUID.Types (UUID, toString)
import Data.Time (ZonedTime)
import Dhall as D
import Data.Text as T
import Config

-- | ユーザー登録関数
--
-- `createuser` オプションを追加することでユーザー登録機能を呼び出します。
mkUser :: FilePath -> IO ()
mkUser confPath = do
  ext <- D.input D.auto $ T.pack confPath
  conn <- connect $ setConnectInfo $ dbInfo ext
  putStrLn "Start user registration."
  putStrLn "input User code (Up to 16 characters of numbers and alphabets): "
  code <- getLine
  putStrLn "input User name: "
  name <- getLine
  putStrLn "input Email address: "
  mail <- getLine
  putStrLn "input Password: "
  pass <- getLine
  createUser conn (passwordSalt ext) code name mail pass

type Code = String
type Pass = String
type Name = String
type Email = String

createUser :: Connection -> Salt -> Code -> Name -> Email -> Pass -> IO ()
createUser
  conn salt code name mail pass = do
  res <- query conn createUserSql (code, name, mail, mkPassword (pack pass, salt))
  if Prelude.null res
    then putStrLn "failed!"
    else do
      putStrLn "Succeeded!!"
      putOut res

  where
    putOut :: [(UUID, ZonedTime)] -> IO ()
    putOut r = do
      let uid = toString $ fst $ Prelude.head r
      putStrLn ("ID: " <> uid)
