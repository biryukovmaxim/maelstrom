module Node
  ( handleRawMessage,
    handleMessage,
    Node (..),
    decodeMessage,
  )
where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromJust)
import Message

data Node = Node
  { nodeID :: String,
    nextMsgID :: Int,
    nodeIDs :: [String],
    customHandler :: String -> MessageBody -> Maybe MessageBody
  }

handleRawMessage :: Node -> BS.ByteString -> Maybe (Node, BS.ByteString)
handleRawMessage node input = do
  message <- decode input
  (newNode, outputMsg) <- handleMessage node message
  Just (newNode, encode outputMsg)

handleMessage :: Node -> Message -> Maybe (Node, Message)
handleMessage node msg@(Message _ _ b) =
  case msgType b of
    InitMessageType -> Just (newNode, newMessage)
      where
        (newNode, newMessage) = handleInitMessage node msg
    CustomMessageType customType -> handleCustomMessage node customType msg
    InitOkType -> Nothing

handleInitMessage :: Node -> Message -> (Node, Message)
handleInitMessage node inputMsg = (newNode, msg)
  where
    inputBody = body inputMsg
    newMsgId = msgId inputBody
    newNode = updateNodeID node (fromJust $ nodeId inputBody) (fromJust $ nodeIds inputBody)
    msgBody = inputBody {msgType = InitOkType, inReplyTo = newMsgId, nodeId = Nothing, nodeIds = Nothing}
    msg = Message {src = nodeID newNode, dest = src inputMsg, body = msgBody}

handleCustomMessage :: Node -> String -> Message -> Maybe (Node, Message)
handleCustomMessage node mt msg = do
  msgBody <- customHandler node mt (body msg)
  let newNode = increaseMsgID node
  return (newNode, Message {src = nodeID node, dest = src msg, body = msgBody {msgId = Just $ nextMsgID newNode}})

updateNodeID :: Node -> String -> [String] -> Node
updateNodeID node newNodeId newNodeIds = node {nodeID = newNodeId, nodeIDs = newNodeIds}

increaseMsgID :: Node -> Node
increaseMsgID n = n {nextMsgID = nextMsgID n + 1}

decodeMessage :: BS.ByteString -> Message
decodeMessage = fromJust . decode
