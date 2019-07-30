module Trillian.Examples.SimpleStorage.Types where

import Data.List (drop, length)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), Options, FromJSON(..), genericToJSON, genericParseJSON, defaultOptions, fieldLabelModifier)


data IncreaseCountTx = IncreaseCountTx
  { increaseCountTxNewCount :: Int
  , increaseCountTxUsername :: String
  } deriving (Eq, Show, Generic)

increaseCountTxOptions :: Options
increaseCountTxOptions = defaultOptions { fieldLabelModifier = drop $ length "increaseCountTx"
                                        }

instance ToJSON IncreaseCountTx where
  toJSON = genericToJSON increaseCountTxOptions

instance FromJSON IncreaseCountTx where
  parseJSON = genericParseJSON increaseCountTxOptions
