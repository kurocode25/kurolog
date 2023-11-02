{-# LANGUAGE DeriveGeneric #-}

module Database.Entity.DBUser (DBUser(..))

where
import Data.UUID.Types (UUID)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data DBUser = DBUser
  { id :: UUID
  , code :: Text
  , name :: Text
  , email :: Text
  , password :: Text
  } deriving (Show, Generic)

instance FromRow DBUser

