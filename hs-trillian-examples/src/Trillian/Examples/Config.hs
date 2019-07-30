module Trillian.Examples.Config where

import           Network.GRPC.Client.Helpers   (GrpcClientConfig,
                                                grpcClientConfigSimple)
import           Trillian.Examples.ConfigUtils (getEnvVar, getEnvVarBool,
                                                makeConfig, readEnvVar)

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
