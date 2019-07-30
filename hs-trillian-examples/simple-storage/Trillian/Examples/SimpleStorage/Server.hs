module Trillian.Examples.SimpleStorage.Server where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Error (runExceptT, either)
import           Data.Proxy
import           Trillian.Examples.Config (makeGrpcClientConfig)
import           Servant
import Network.HTTP2.Client (ClientIO)
import           Network.GRPC.Client.Helpers (setupGrpcClient, GrpcClient)
import           Trillian.Examples.SimpleStorage.Types


type PostIncreaseCountTx =
     "tx"
  :> "increase_count"
  :> ReqBody '[JSON] IncreaseCountTx
  :> Post '[JSON] Hash

type GetIncreaseCountTx =
     "tx"
  :> "increase_count"
  :> QueryParam' '[Required] "hash" Hash
  :> Get '[JSON] IncreaseCountTx

type API =
  PostIncreaseCountTx :<|>
  GetIncreaseCountTx

api :: Proxy API
api = Proxy

--------------------------------------------------------------------------------
-- Server implementation
--------------------------------------------------------------------------------

data AppContext =
  AppContext { grpcClient :: GrpcClient
             }

makeAppContext :: IO AppContext
makeAppContext = do
  grpcCfg <- makeGrpcClientConfig
  egrpc <- runExceptT $ setupGrpcClient grpcCfg
  grpc <- either (\e -> error $ "Error intiiating GrpcClient " <> show e) pure egrpc
  pure $ AppContext { grpcClient = grpc
                    }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
transformClientIO :: forall a. ClientIO a -> Handler a
transformClientIO cio = do
  ea <- liftIO $ runExceptT cio
  case ea of
    Left _ -> throwError err500 {errBody = "EarlyEndOfStream"}
    Right a -> pure a
