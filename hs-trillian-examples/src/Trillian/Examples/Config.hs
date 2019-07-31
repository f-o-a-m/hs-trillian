module Trillian.Examples.Config where

import           Control.Error                     (runExceptT)
import           Control.Lens                      ((&), (.~), (^.))
import qualified Data.ByteString.Base16            as BS16
import           Data.Int                          (Int64)
import           Data.ProtoLens.Message            (Message (defMessage))
import           Data.String.Conversions           (cs)
import           Network.GRPC.Client               (RawReply, uncompressed)
import           Network.GRPC.Client.Helpers       (GrpcClient,
                                                    GrpcClientConfig,
                                                    grpcClientConfigSimple,
                                                    _grpcClientConfigCompression)
import           Network.HTTP2.Client              (TooMuchConcurrency)
import           Proto.Crypto.Keyspb.Keyspb        as Keyspb
import           Proto.Crypto.Keyspb.Keyspb_Fields as Keyspb
import           Proto.Crypto.Sigpb.Sigpb          as Sigpb
import qualified Proto.Trillian                    as T
import qualified Proto.Trillian_Fields             as T
import qualified Proto.TrillianAdminApi_Fields     as AdminApi
import qualified Proto.TrillianLogApi_Fields       as LogApi
import qualified Trillian.Admin.RPCCall            as AdminRPC
import           Trillian.Examples.ConfigUtils     (getEnvVar, getEnvVarBool,
                                                    makeConfig, readEnvVar)
import qualified Trillian.Log.RPCCall              as LogRPC


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

createTrillianLog :: GrpcClient -> IO Int64
createTrillianLog grpc = do
  erespLogId <- runExceptT $ do
    let treeReq =
          defMessage & AdminApi.tree .~ defaultLogTree
                     & AdminApi.keySpec .~ defaultKeySpec
    etree <- formatResponse "CreateTreeRequest " <$> AdminRPC.createTree grpc treeReq
    tree <- either error pure etree
    let logId = tree ^. T.treeId
        logReqMsg = defMessage &  LogApi.logId .~ logId
    elogResp <- formatResponse "InitLogRequest " <$> LogRPC.initLog grpc logReqMsg
    logResp <- either error pure elogResp
    pure (logResp, logId)
  case erespLogId of
    Left e -> error $ show e
    Right (resp, logId) ->
      let prefix = "Log created with logId " <> show logId
      in case resp ^. LogApi.maybe'created of
           Nothing -> do
             putStrLn $ prefix <> "<Empty InitLogResponse>"
             pure logId
           Just signedLogRoot ->
             let toHex = cs . BS16.encode
                 msg = "logRoot=" <> toHex (signedLogRoot ^. T.logRoot) <> ", logId=" <> show logId
             in do
               putStrLn $ prefix <> msg
               pure logId
  where
    formatResponse :: String
                   -> Either TooMuchConcurrency (
                        RawReply a
                      )
                   -> Either String a
    formatResponse errorPrefix (Left _) = Left $ errorPrefix <> "TooMuchConcurrency"
    formatResponse errorPrefix (Right rr) = case rr of
      Left errCode -> Left $ errorPrefix <> "Error Code " <> show errCode
      Right (_,_,ea) -> case ea of
        Left e  -> Left $ errorPrefix <> e
        Right a -> Right a

defaultLogTree :: T.Tree
defaultLogTree = defMessage
  & T.treeState .~ T.ACTIVE
  & T.treeType .~ T.LOG
  & T.hashStrategy .~ T.RFC6962_SHA256
  & T.hashAlgorithm .~ Sigpb.DigitallySigned'SHA256
  & T.signatureAlgorithm .~ Sigpb.DigitallySigned'ECDSA
  & T.maxRootDuration .~ defMessage

defaultKeySpec :: Keyspb.Specification
defaultKeySpec =
  defMessage
    & Keyspb.ecdsaParams .~ ( defMessage
                                & Keyspb.curve .~ Keyspb.Specification'ECDSA'P256
                            )
