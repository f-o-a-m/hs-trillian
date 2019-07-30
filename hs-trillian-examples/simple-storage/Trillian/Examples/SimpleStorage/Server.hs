module Trillian.Examples.SimpleStorage.Server where

import           Control.Error                         (runExceptT)
import           Control.Lens                          ((&), (.~), (^.))
import           Control.Monad.Error.Class             (throwError)
import           Control.Monad.IO.Class                (liftIO)
import qualified Data.Aeson                            as AE
import           Data.Int                              (Int64)
import           Data.ProtoLens.Message                (Message (defMessage))
import           Data.Proxy
import           Data.String.Conversions               (cs)
import           Network.GRPC.Client                   (RawReply)
import           Network.GRPC.Client.Helpers           (GrpcClient,
                                                        setupGrpcClient)
import           Network.HTTP2.Client                  (ClientIO,
                                                        TooMuchConcurrency)
import qualified Proto.TrillianLogApi_Fields           as TApi
import           Servant
import           Trillian.Examples.Config              (makeGrpcClientConfig)
import           Trillian.Examples.ConfigUtils         (makeConfig, readEnvVar)
import           Trillian.Examples.SimpleStorage.Types
import qualified Trillian.Log.RPCCall                  as LogRPC


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

-- Context

data AppContext =
  AppContext { grpcClient :: GrpcClient
             , logId      :: Int64
             }

makeAppContext :: IO AppContext
makeAppContext = do
  grpcCfg <- makeGrpcClientConfig
  grpc <- do
    ec <- runExceptT $ setupGrpcClient grpcCfg
    case ec of
      Left e     -> error $ "Error intiiating GrpcClient " <> show e
      Right grpc -> return grpc
  lid <- makeConfig $ readEnvVar "TRILLIAN_LOG_ID"
  pure $ AppContext { grpcClient = grpc
                    , logId = lid
                    }

-- Handlers

postIncreaseCountTx :: AppContext -> IncreaseCountTx -> Handler Hash
postIncreaseCountTx AppContext{grpcClient, logId} tx = do
  let txBytes = cs $ AE.encode tx
      leaf =
        defMessage
          & TApi.leafValue .~ txBytes
      logRequest =
        defMessage
          & TApi.logId .~ logId
          & TApi.leaf .~ leaf
  logResp <- transformClientIO $
    LogRPC.queueLeaf grpcClient logRequest
  pure . Hash $
    logResp ^. TApi.queuedLeaf . TApi.leaf . TApi.leafValue

getIncreaseCountTx :: AppContext -> Hash -> Handler IncreaseCountTx
getIncreaseCountTx AppContext{grpcClient, logId} (Hash txBytes) = do
  let reqMsg =
        defMessage
          & TApi.logId .~ logId
          & TApi.leafHash .~ [txBytes]
  logResp <- transformClientIO $
    LogRPC.getLeavesByHash grpcClient reqMsg
  case logResp ^. TApi.leaves of
    [leaf] -> case  AE.eitherDecodeStrict $  leaf ^. TApi.leafValue of
      Left e -> throwError err500 {errBody = cs $ "Error decoding leaf from store : " <> e}
      Right a -> return a
    as -> throwError err500 {errBody = cs $ "Got unexpected number of leaves from storage, expecting 1 got " <> show (length as) }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
transformClientIO
  :: forall a.
     ClientIO (Either TooMuchConcurrency (RawReply a))
  -> Handler a
transformClientIO cio = do
  eResp <- liftIO $ runExceptT cio
  case eResp of
    Left _ -> throwError err500 {errBody = "EarlyEndOfStream"}
    Right resp -> case resp of
      Left _ -> throwError err500 {errBody = "TooMuchConcurrency"}
      Right rr -> case rr of
        Left ec -> throwError err500 { errBody = cs $ "RPCError with code " <> show ec}
        Right (_,_,ea) ->
          case ea of
            Left errMsg -> throwError err500 { errBody = cs $ "RPCError: " <> errMsg }
            Right a -> pure a
