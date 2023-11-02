module View.Class where

import Data.Text
import Control.Monad.Trans.Reader
import Config

class ToSitemap a where
  serialize :: a -> Reader Config (Text, Text, Text)

