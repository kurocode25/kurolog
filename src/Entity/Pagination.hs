{-# LANGUAGE DeriveGeneric #-}

module Entity.Pagination (Pagination(..), CurrentPage, Limit, Total) 
where

import Data.Aeson
import GHC.Generics (Generic)


data Pagination = Pagination
  { current :: CurrentPage
  , totalPages :: Total
  , next :: Maybe Int
  , back :: Maybe Int
  , limit :: Limit
  } deriving (Show, Generic)

instance ToJSON Pagination

type CurrentPage = Int
type Limit = Int
type Total = Int

