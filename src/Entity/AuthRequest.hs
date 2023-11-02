{-# LANGUAGE DeriveGeneric #-}

module Entity.AuthRequest  where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data AuthRequest = AuthRequest
  { username :: Text
  , password :: Text 
  } deriving (Show, Generic)

instance FromJSON AuthRequest