module Trillian.Log.RPCCall where

import           Network.GRPC.Client         (RPC (..), RawReply)
import           Network.GRPC.Client.Helpers (GrpcClient, rawUnary)
import           Network.HTTP2.Client        (ClientIO, TooMuchConcurrency)
import qualified Proto.TrillianLogApi        as Api

initLog
  :: GrpcClient
  -> Api.InitLogRequest
  -> ClientIO (Either TooMuchConcurrency (RawReply Api.InitLogResponse))
initLog = rawUnary (RPC :: RPC Api.TrillianLog "initLog")

queueLeaf
  :: GrpcClient
  -> Api.QueueLeafRequest
  -> ClientIO (Either TooMuchConcurrency (RawReply Api.QueueLeafResponse))
queueLeaf = rawUnary (RPC :: RPC Api.TrillianLog "queueLeaf")

{-
queueLeaves
  :: Api.QueueLeavesRequest
  -> RPCCall Api.TrillianLog "queueLeaves" (RawReply Api.QueueLeavesResponse)
queueLeaves = singleRequest (RPC :: RPC Api.TrillianLog "queueLeaves")

getSequencedLeafCount
  :: Api.GetSequencedLeafCountRequest
  -> RPCCall Api.TrillianLog "getSequencedLeafCount" (RawReply Api.GetSequencedLeafCountResponse)
getSequencedLeafCount = singleRequest (RPC :: RPC Api.TrillianLog "getSequencedLeafCount")

addSequencedLeaf
  :: Api.AddSequencedLeafRequest
  -> RPCCall Api.TrillianLog "addSequencedLeaf" (RawReply Api.AddSequencedLeafResponse)
addSequencedLeaf = singleRequest (RPC :: RPC Api.TrillianLog "addSequencedLeaf")

addSequencedLeaves
  :: Api.AddSequencedLeavesRequest
  -> RPCCall Api.TrillianLog "addSequencedLeaves" (RawReply Api.AddSequencedLeavesResponse)
addSequencedLeaves = singleRequest (RPC :: RPC Api.TrillianLog "addSequencedLeaves")

-}

getLeavesByHash
  :: GrpcClient
  -> Api.GetLeavesByHashRequest
  -> ClientIO (Either TooMuchConcurrency (RawReply Api.GetLeavesByHashResponse))
getLeavesByHash = rawUnary (RPC :: RPC Api.TrillianLog "getLeavesByHash")

{-
getLeavesByIndex
  :: Api.GetLeavesByIndexRequest
  -> RPCCall Api.TrillianLog "getLeavesByIndex" (RawReply Api.GetLeavesByIndexResponse)
getLeavesByIndex = singleRequest (RPC :: RPC Api.TrillianLog "getLeavesByIndex")

getInclusionProof
  :: Api.GetInclusionProofRequest
  -> RPCCall Api.TrillianLog "getInclusionProof" (RawReply Api.GetInclusionProofResponse)
getInclusionProof = singleRequest (RPC :: RPC Api.TrillianLog "getInclusionProof")

getInclusionProofByHash
  :: Api.GetInclusionProofByHashRequest
  -> RPCCall Api.TrillianLog "getInclusionProofByHash" (RawReply Api.GetInclusionProofByHashResponse)
getInclusionProofByHash = singleRequest (RPC :: RPC Api.TrillianLog "getInclusionProofByHash")

getConsistencyProof
  :: Api.GetConsistencyProofRequest
  -> RPCCall Api.TrillianLog "getConsistencyProof" (RawReply Api.GetConsistencyProofResponse)
getConsistencyProof = singleRequest (RPC :: RPC Api.TrillianLog "getConsistencyProof")

getLatestSignedLogRoot
  :: Api.GetLatestSignedLogRootRequest
  -> RPCCall Api.TrillianLog "getLatestSignedLogRoot" (RawReply Api.GetLatestSignedLogRootResponse)
getLatestSignedLogRoot = singleRequest (RPC :: RPC Api.TrillianLog "getLatestSignedLogRoot")

getEntryAndProof
  :: Api.GetEntryAndProofRequest
  -> RPCCall Api.TrillianLog "getEntryAndProof" (RawReply Api.GetEntryAndProofResponse)
getEntryAndProof = singleRequest (RPC :: RPC Api.TrillianLog "getEntryAndProof")
-}
