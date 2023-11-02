{-# LANGUAGE DeriveGeneric #-}
module Entity.ResultResponse (ResultResponse(..))
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data ResultResponse =
  ResultResponse
    { success :: Bool
    , message :: Text
    } deriving (Show, Generic)

instance ToJSON ResultResponse

