module Trillian.Log.Handlers where

import           Data.ProtoLens.Message       (Message (defMessage))
import           Network.GRPC.Server.Handlers (UnaryHandler)
import           Proto.TrillianLogApi         (TrillianLog)


handleInitLog :: UnaryHandler TrillianLog "initLog"
handleInitLog _ _ = return defMessage

handleQueueLeaf :: UnaryHandler TrillianLog "queueLeaf"
handleQueueLeaf _ _ = return defMessage

handleQueueLeaves :: UnaryHandler TrillianLog "queueLeaves"
handleQueueLeaves _ _ = return defMessage

handleGetSequencedLeafCount :: UnaryHandler TrillianLog "getSequencedLeafCount"
handleGetSequencedLeafCount _ _ = return defMessage

handleAddSequencedLeaf :: UnaryHandler TrillianLog "addSequencedLeaf"
handleAddSequencedLeaf _ _ = return defMessage

handleAddSequencedLeaves :: UnaryHandler TrillianLog "addSequencedLeaves"
handleAddSequencedLeaves _ _ = return defMessage

handleGetLeavesByHash :: UnaryHandler TrillianLog "getLeavesByHash"
handleGetLeavesByHash _ _ = return defMessage

handleGetLeavesByIndex :: UnaryHandler TrillianLog "getLeavesByIndex"
handleGetLeavesByIndex _ _ = return defMessage

handleGetLeavesByRange :: UnaryHandler TrillianLog "getLeavesByRange"
handleGetLeavesByRange _ _ = return defMessage

handleGetInclusionProof :: UnaryHandler TrillianLog "getInclusionProof"
handleGetInclusionProof _ _ = return defMessage

handleGetInclusionProofByHash :: UnaryHandler TrillianLog "getInclusionProofByHash"
handleGetInclusionProofByHash _ _ = return defMessage

handleGetConsistencyProof :: UnaryHandler TrillianLog "getConsistencyProof"
handleGetConsistencyProof _ _ = return defMessage

handleGetLatestSignedLogRoot :: UnaryHandler TrillianLog "getLatestSignedLogRoot"
handleGetLatestSignedLogRoot _ _ = return defMessage

handleGetEntryAndProof :: UnaryHandler TrillianLog "getEntryAndProof"
handleGetEntryAndProof _ _ = return defMessage
