module Trillian.Log.RPCCall where

import           Network.GRPC.Client (RPCCall, RawReply, RPC(..), singleRequest)
import qualified Proto.TrillianLogApi as Api

initLog
  :: Api.InitLogRequest
  -> RPCCall Api.TrillianLog "initLog" (RawReply Api.InitLogResponse)
initLog = singleRequest (RPC :: RPC Api.TrillianLog "initLog")

queueLeaf
  :: Api.QueueLeafRequest
  -> RPCCall Api.TrillianLog "queueLeaf" (RawReply Api.QueueLeafResponse)
queueLeaf = singleRequest (RPC :: RPC Api.TrillianLog "queueLeaf")

queueLeaves
  :: Api.QueueLeavesRequest
  -> RPCCall Api.TrillianLog "queueLeaves" (RawReply Api.QueueLeavesResponse)
queueLeaves = singleRequest (RPC :: RPC Api.TrillianLog "queueLeaves")

getSequencedLeafCount
  :: Api.GetSequencedLeafCountRequest
  -> RPCCall Api.TrillianLog "getSequencedLeafCount" (RawReply Api.GetSequencedLeafCountResponse)
getSequencedLeafCount = singleRequest (RPC :: RPC Api.TrillianLog "getSequencedLeafCount")
--
--addSequencedLeaf :: UnaryHandler TrillianLog "addSequencedLeaf"
--addSequencedLeaf _ _ = return defMessage
--
--addSequencedLeaves :: UnaryHandler TrillianLog "addSequencedLeaves"
--addSequencedLeaves _ _ = return defMessage
--
--getLeavesByHash :: UnaryHandler TrillianLog "getLeavesByHash"
--getLeavesByHash _ _ = return defMessage
--
--getLeavesByIndex :: UnaryHandler TrillianLog "getLeavesByIndex"
--getLeavesByIndex _ _ = return defMessage
--
--getLeavesByRange :: UnaryHandler TrillianLog "getLeavesByRange"
--getLeavesByRange _ _ = return defMessage
--
--getInclusionProof :: UnaryHandler TrillianLog "getInclusionProof"
--getInclusionProof _ _ = return defMessage
--
--getInclusionProofByHash :: UnaryHandler TrillianLog "getInclusionProofByHash"
--getInclusionProofByHash _ _ = return defMessage
--
--getConsistencyProof :: UnaryHandler TrillianLog "getConsistencyProof"
--getConsistencyProof _ _ = return defMessage

--getLatestSignedLogRoot :: UnaryHandler TrillianLog "getLatestSignedLogRoot"
--getLatestSignedLogRoot _ _ = return defMessage
--
--getEntryAndProof :: UnaryHandler TrillianLog "getEntryAndProof"
--getEntryAndProof _ _ = return defMessage
