module Trillian.Examples.Config where

import Trillian.Examples.ConfigUtils (makeConfig, getEnvVar, readEnvVar, getEnvVarBool)
import Network.GRPC.Client.Helpers (GrpcClientConfig, grpcClientConfigSimple)

makeGrpcClientConfig :: IO GrpcClientConfig
makeGrpcClientConfig = do
  (hostName, portNumber, useTLS) <-
    makeConfig $ do
      hn <- getEnvVar "TRILLIAN_HOST_NAME"
      p <- readEnvVar "TRILLIAN_PORT"
      tls <- getEnvVarBool "TRILLIAN_USE_TLS"
      return (hn, p, tls)
  return $
    grpcClientConfigSimple hostName portNumber useTLS
