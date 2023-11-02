{-# LANGUAGE OverloadedStrings #-}

module Utils (mkPassword, tz2text, tzstamp2tz, convertReleasedAtTime, Salt) where

import Data.Text (Text, pack)
import Data.Text.Encoding(encodeUtf8)
import Data.Time.Format
import Data.Time
import Crypto.Hash (hash, Digest, MD5, SHA256)
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple.Time (ZonedTimestamp, Unbounded(..))


type Salt = Text
type Password = Text

-- |create password function
--
mkPassword :: (Text, Salt) -> Password
mkPassword (input, s) =
  pack $ show $ sha256 $ encodeUtf8 $ pack (show $ md5 ctx) <> s
  where
    md5 :: ByteString -> Digest MD5
    md5 = hash
    sha256 :: ByteString -> Digest SHA256
    sha256 = hash
    ctx = encodeUtf8 (input <> s)

-- | convert ZonedTime to Text
--
-- Text format is "%Y-%m-%d"
tz2text :: ZonedTime -> Text
tz2text zt =
  pack $ formatTime defaultTimeLocale "%Y-%m-%d" zt
  
tzstamp2tz :: ZonedTimestamp -> ZonedTime
tzstamp2tz (Finite zt) = zt
tzstamp2tz NegInfinity = read "2023-10-00 01:00:00.000000+09"
tzstamp2tz PosInfinity = read "2023-10-00 01:00:00.000000+09"

-- | convert ZonedTimestamp to ZonedTime
convertReleasedAtTime :: Maybe ZonedTimestamp -> Maybe ZonedTime
convertReleasedAtTime input =
  case input of
    (Just (Finite zt)) -> Just zt
    _ -> Nothing
