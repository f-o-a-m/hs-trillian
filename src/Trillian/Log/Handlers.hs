module Trillian.Log.Handlers where

import  Network.GRPC.Server.Handlers (UnaryHandler)
import           Data.ProtoLens.Message (Message(defMessage))
import Proto.TrillianLogApi (TrillianLog)

{-

data TrillianLog = TrillianLog{}
                     deriving ()
instance Data.ProtoLens.Service.Types.Service TrillianLog where
        type ServiceName TrillianLog = "TrillianLog"
        type ServicePackage TrillianLog = "trillian"
        type ServiceMethods TrillianLog =
             '["addSequencedLeaf", "addSequencedLeaves", "getConsistencyProof",
               "getEntryAndProof", "getInclusionProof", "getInclusionProofByHash",
               "getLatestSignedLogRoot", "getLeavesByHash", "getLeavesByIndex",
               "getLeavesByRange", "getSequencedLeafCount", "initLog",
               "queueLeaf", "queueLeaves"]
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "queueLeaf"
         where
        type MethodName TrillianLog "queueLeaf" = "QueueLeaf"
        type MethodInput TrillianLog "queueLeaf" = QueueLeafRequest
        type MethodOutput TrillianLog "queueLeaf" = QueueLeafResponse
        type MethodStreamingType TrillianLog "queueLeaf" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "addSequencedLeaf"
         where
        type MethodName TrillianLog "addSequencedLeaf" = "AddSequencedLeaf"
        type MethodInput TrillianLog "addSequencedLeaf" =
             AddSequencedLeafRequest
        type MethodOutput TrillianLog "addSequencedLeaf" =
             AddSequencedLeafResponse
        type MethodStreamingType TrillianLog "addSequencedLeaf" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "getInclusionProof"
         where
        type MethodName TrillianLog "getInclusionProof" =
             "GetInclusionProof"
        type MethodInput TrillianLog "getInclusionProof" =
             GetInclusionProofRequest
        type MethodOutput TrillianLog "getInclusionProof" =
             GetInclusionProofResponse
        type MethodStreamingType TrillianLog "getInclusionProof" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "getInclusionProofByHash"
         where
        type MethodName TrillianLog "getInclusionProofByHash" =
             "GetInclusionProofByHash"
        type MethodInput TrillianLog "getInclusionProofByHash" =
             GetInclusionProofByHashRequest
        type MethodOutput TrillianLog "getInclusionProofByHash" =
             GetInclusionProofByHashResponse
        type MethodStreamingType TrillianLog "getInclusionProofByHash" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "getConsistencyProof"
         where
        type MethodName TrillianLog "getConsistencyProof" =
             "GetConsistencyProof"
        type MethodInput TrillianLog "getConsistencyProof" =
             GetConsistencyProofRequest
        type MethodOutput TrillianLog "getConsistencyProof" =
             GetConsistencyProofResponse
        type MethodStreamingType TrillianLog "getConsistencyProof" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "getLatestSignedLogRoot"
         where
        type MethodName TrillianLog "getLatestSignedLogRoot" =
             "GetLatestSignedLogRoot"
        type MethodInput TrillianLog "getLatestSignedLogRoot" =
             GetLatestSignedLogRootRequest
        type MethodOutput TrillianLog "getLatestSignedLogRoot" =
             GetLatestSignedLogRootResponse
        type MethodStreamingType TrillianLog "getLatestSignedLogRoot" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "getSequencedLeafCount"
         where
        type MethodName TrillianLog "getSequencedLeafCount" =
             "GetSequencedLeafCount"
        type MethodInput TrillianLog "getSequencedLeafCount" =
             GetSequencedLeafCountRequest
        type MethodOutput TrillianLog "getSequencedLeafCount" =
             GetSequencedLeafCountResponse
        type MethodStreamingType TrillianLog "getSequencedLeafCount" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "getEntryAndProof"
         where
        type MethodName TrillianLog "getEntryAndProof" = "GetEntryAndProof"
        type MethodInput TrillianLog "getEntryAndProof" =
             GetEntryAndProofRequest
        type MethodOutput TrillianLog "getEntryAndProof" =
             GetEntryAndProofResponse
        type MethodStreamingType TrillianLog "getEntryAndProof" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "initLog"
         where
        type MethodName TrillianLog "initLog" = "InitLog"
        type MethodInput TrillianLog "initLog" = InitLogRequest
        type MethodOutput TrillianLog "initLog" = InitLogResponse
        type MethodStreamingType TrillianLog "initLog" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "queueLeaves"
         where
        type MethodName TrillianLog "queueLeaves" = "QueueLeaves"
        type MethodInput TrillianLog "queueLeaves" = QueueLeavesRequest
        type MethodOutput TrillianLog "queueLeaves" = QueueLeavesResponse
        type MethodStreamingType TrillianLog "queueLeaves" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "addSequencedLeaves"
         where
        type MethodName TrillianLog "addSequencedLeaves" =
             "AddSequencedLeaves"
        type MethodInput TrillianLog "addSequencedLeaves" =
             AddSequencedLeavesRequest
        type MethodOutput TrillianLog "addSequencedLeaves" =
             AddSequencedLeavesResponse
        type MethodStreamingType TrillianLog "addSequencedLeaves" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "getLeavesByIndex"
         where
        type MethodName TrillianLog "getLeavesByIndex" = "GetLeavesByIndex"
        type MethodInput TrillianLog "getLeavesByIndex" =
             GetLeavesByIndexRequest
        type MethodOutput TrillianLog "getLeavesByIndex" =
             GetLeavesByIndexResponse
        type MethodStreamingType TrillianLog "getLeavesByIndex" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "getLeavesByRange"
         where
        type MethodName TrillianLog "getLeavesByRange" = "GetLeavesByRange"
        type MethodInput TrillianLog "getLeavesByRange" =
             GetLeavesByRangeRequest
        type MethodOutput TrillianLog "getLeavesByRange" =
             GetLeavesByRangeResponse
        type MethodStreamingType TrillianLog "getLeavesByRange" =
             'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TrillianLog
           "getLeavesByHash"
         where
        type MethodName TrillianLog "getLeavesByHash" = "GetLeavesByHash"
        type MethodInput TrillianLog "getLeavesByHash" =
             GetLeavesByHashRequest
        type MethodOutput TrillianLog "getLeavesByHash" =
             GetLeavesByHashResponse
        type MethodStreamingType TrillianLog "getLeavesByHash" =
             'Data.ProtoLens.Service.Types.NonStreaming%

-}

handleQueueLeaf :: UnaryHandler TrillianLog "queueLeaf"
handleQueueLeaf _ _ = return $ defMessage
