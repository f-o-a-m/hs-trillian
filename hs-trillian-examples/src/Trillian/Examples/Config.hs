module Trillian.Examples.Config where

import           Control.Error                 (runExceptT)
import           Control.Lens                  ((&), (.~), (^.))
import qualified Data.ByteString.Base16        as BS16
import           Data.Int                      (Int64)
import           Data.ProtoLens.Message        (Message (defMessage))
import           Data.String.Conversions       (cs)
import           Network.GRPC.Client           (RawReply, uncompressed)
import           Network.GRPC.Client.Helpers   (GrpcClient, GrpcClientConfig,
                                                grpcClientConfigSimple,
                                                _grpcClientConfigCompression)
import           Network.HTTP2.Client          (ClientError, TooMuchConcurrency)
import qualified Proto.Trillian_Fields         as T
import qualified Proto.TrillianLogApi          as TApi
import qualified Proto.TrillianLogApi_Fields   as TApi
import           Trillian.Examples.ConfigUtils (getEnvVar, getEnvVarBool,
                                                makeConfig, readEnvVar)
import qualified Trillian.Log.RPCCall          as LogRPC


makeGrpcClientConfig :: IO GrpcClientConfig
makeGrpcClientConfig = do
  (hostName, portNumber, useTLS) <-
    makeConfig $ do
      hn <- getEnvVar "TRILLIAN_HOST_NAME"
      p <- readEnvVar "TRILLIAN_PORT"
      tls <- getEnvVarBool "TRILLIAN_USE_TLS"
      return (hn, p, tls)
  let grpcCfg = grpcClientConfigSimple hostName portNumber useTLS
  return grpcCfg {_grpcClientConfigCompression = uncompressed}

createTrillianLog :: GrpcClient -> Int64 -> IO ()
createTrillianLog grpc logId = do
  let reqMsg = defMessage &  TApi.logId .~ logId
  eresp <- runExceptT $ LogRPC.initLog grpc reqMsg
  case formatResponse eresp of
    Left e -> error e
    Right resp ->
      let prefix = "Log created with logId " <> show logId <> " : "
      in case resp ^. TApi.maybe'created of
           Nothing -> putStrLn $ prefix <> "<Empty InitLogResponse>"
           Just signedLogRoot ->
             let toHex = cs . BS16.encode
                 msg = "logRoot=" <> toHex (signedLogRoot ^. T.logRoot)
             in putStrLn $ prefix <> msg
  where
    errorPrefix = "Error in InitLog request : "
    formatResponse :: Either ClientError (
                        Either TooMuchConcurrency (
                          RawReply TApi.InitLogResponse
                        )
                      )
                   -> Either String TApi.InitLogResponse
    formatResponse (Left e) = Left $ errorPrefix <> "ClientError " <> show e
    formatResponse (Right (Left _)) = Left $ errorPrefix <> "TooMuchConcurrency"
    formatResponse (Right (Right rr)) = case rr of
      Left errCode -> Left $ errorPrefix <> "Error Code " <> show errCode
      Right (_,_,ea) -> case ea of
        Left e  -> Left $ errorPrefix <> e
        Right a -> Right a
