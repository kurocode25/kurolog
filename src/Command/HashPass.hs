module Command.HashPass
where

import Utils
import Data.Text as T
import Dhall
import Config

hashPass :: FilePath -> String -> IO ()
hashPass path pass = do
  ex <- input auto $ T.pack path :: IO ExtConfig
  putStrLn $ h $ passwordSalt ex
    where
      h salt = T.unpack $ mkPassword (T.pack pass, salt)

