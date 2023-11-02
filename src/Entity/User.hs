{-# LANGUAGE DeriveGeneric #-}
module Entity.User (User(..))
where

import Data.UUID.Types (UUID)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson

data User = User
  { id :: Maybe UUID -- in case of create user, id is null
  , code :: Text
  , name :: Text
  , email :: Text
  , password :: Maybe Text -- for response, password is null
  } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User