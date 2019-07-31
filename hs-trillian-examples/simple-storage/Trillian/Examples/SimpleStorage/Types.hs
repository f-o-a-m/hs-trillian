module Trillian.Examples.SimpleStorage.Types
  ( IncreaseCountTx(..)
  , Hash(..)
  ) where

import           Data.Aeson              (FromJSON (..), Options, ToJSON (..),
                                          defaultOptions, fieldLabelModifier,
                                          genericParseJSON, genericToJSON,
                                          withText)
import           Data.Bifunctor          (first)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Base16  as B16
import           Data.Char               (toLower)
import           Data.List               (drop, length)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           GHC.Generics            (Generic)
import           Servant                 (FromHttpApiData (..))


data IncreaseCountTx = IncreaseCountTx
  { increaseCountTxNewCount :: Int
  , increaseCountTxUsername :: String
  } deriving (Eq, Show, Generic)

increaseCountTxOptions :: Options
increaseCountTxOptions =
  defaultOptions { fieldLabelModifier = lowerFirst . drop (length @[] @Char "increaseCountTx")
                 }

instance ToJSON IncreaseCountTx where
  toJSON = genericToJSON increaseCountTxOptions

instance FromJSON IncreaseCountTx where
  parseJSON = genericParseJSON increaseCountTxOptions

newtype Hash = Hash ByteString

parseHash :: Text -> Either String Hash
parseHash t =
  let (hex, rest) = B16.decode . encodeUtf8 $ t
  in if rest == mempty
       then Right (Hash hex)
       else Left $ "Encountered non base-16 encoded bytes: " <> show rest

instance FromHttpApiData Hash where
  parseQueryParam = first cs . parseHash

instance ToJSON Hash where
  toJSON (Hash bs) = toJSON . decodeUtf8 . B16.encode $ bs

instance FromJSON Hash where
  parseJSON = withText "Couldn't parse Hash" $ \t ->
    case parseHash t of
      Left err   -> fail err
      Right hash -> pure hash

lowerFirst :: String -> String
lowerFirst []       = []
lowerFirst (a : as) = toLower a : as

