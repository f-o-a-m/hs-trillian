module Trillian.Admin.RPCCall where

import           Network.GRPC.Client         (RPC (..), RawReply)
import           Network.GRPC.Client.Helpers (GrpcClient, rawUnary)
import           Network.HTTP2.Client        (ClientIO, TooMuchConcurrency)
import qualified Proto.Trillian              as Trillian
import qualified Proto.TrillianAdminApi      as Api

createTree
  :: GrpcClient
  -> Api.CreateTreeRequest
  -> ClientIO (Either TooMuchConcurrency (RawReply Trillian.Tree))
createTree = rawUnary (RPC :: RPC Api.TrillianAdmin "createTree")
