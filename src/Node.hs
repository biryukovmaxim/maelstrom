module Node
  ( handleRawMessage,
    Node (Node),
    emptyNode,
  )
where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromJust)
import Message

data Node = Node
  { nodeID :: String,
    nextMsgID :: Int,
    nodeIDs :: [String]
  }
  deriving (Show)

emptyNode :: Node
emptyNode = Node {
  nodeID = "",
  nextMsgID = 0,
  nodeIDs = []
}

handleRawMessage :: Node -> BS.ByteString -> Maybe (Node, BS.ByteString)
handleRawMessage node input = do
  message <- decode input
  (newNode, outputMsg) <- handleMessage node message
  Just (newNode, encode outputMsg)

handleMessage :: Node -> Message -> Maybe (Node, Message)
handleMessage node msg@(Message _ _ (Just b)) =
  case messageType b of
    InitMessageType -> Just (newNode, msg {body = body newMessage})
      where (newNode, newMessage) = handleInitMessage node msg
    CustomMessageType _ -> Just (node, msg)
    InitOkType -> Nothing
handleMessage node msg = Just (node, msg)

handleInitMessage :: Node -> Message -> (Node, Message)
handleInitMessage node inputMsg = (newNode, msg)
  where
     inputBody = fromJust $ body inputMsg
     newMsgId = msgID inputBody
     newNode = updateNode node (fromJust $ nodeId inputBody) (fromJust $ nodeIds inputBody)
     msgBody = inputBody {messageType = InitOkType, inReplyTo = newMsgId, nodeId = Nothing, nodeIds = Nothing}
     msg = Message { src = Just $ nodeID newNode, dest = src inputMsg, body = Just msgBody }

updateNode :: Node -> String -> [String] -> Node
updateNode node newNodeId newNodeIds = node {nodeID = newNodeId, nodeIDs = newNodeIds}
