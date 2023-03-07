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

data Node a = Node
  { nodeID :: String,
    nextMsgID :: Int,
    nodeIDs :: [String],
    state :: a,
    customHandler :: String -> a -> MessageBody -> IO (Maybe (a, MessageBody))
  }

handleRawMessage :: Node a -> BS.ByteString -> IO (Maybe (Node a, BS.ByteString))
handleRawMessage node input = do
  case decode input of
    Nothing -> pure Nothing
    Just message -> do
      res <- handleMessage node message
      case res of
        Nothing -> pure Nothing
        Just (newNode, outputMsg) -> pure $ Just (newNode, encode outputMsg)

handleMessage :: Node a -> Message -> IO (Maybe (Node a, Message))
handleMessage node msg@(Message _ _ b) =
  case msgType b of
    InitMessageType -> pure $ Just (newNode, newMessage)
      where
        (newNode, newMessage) = handleInitMessage node msg
    CustomMessageType customType -> handleCustomMessage node customType msg
    InitOkType -> pure Nothing

handleInitMessage :: Node a -> Message -> (Node a, Message)
handleInitMessage node inputMsg = (newNode, msg)
  where
    inputBody = body inputMsg
    newMsgId = msgId inputBody
    newNode = updateNodeID node (fromJust $ nodeId inputBody) (fromJust $ nodeIds inputBody)
    msgBody = inputBody {msgType = InitOkType, inReplyTo = newMsgId, nodeId = Nothing, nodeIds = Nothing}
    msg = Message {src = nodeID newNode, dest = src inputMsg, body = msgBody}

handleCustomMessage :: Node a -> String -> Message -> IO (Maybe (Node a, Message))
handleCustomMessage node mt msg = do
  let handler = customHandler node
  let state' = state node
  mbStateMsgBody <- handler mt state' (body msg)
  let newNode = increaseMsgID node
  let replyTo = msgId $ body msg
  case mbStateMsgBody of
    Just (newState, msgBody) -> pure $ Just (newNode {state = newState}, Message {src = nodeID node, dest = src msg, body = msgBody {msgId = Just $ nextMsgID newNode, inReplyTo = replyTo}})
    Nothing -> pure Nothing

updateNodeID :: Node a -> String -> [String] -> Node a
updateNodeID node newNodeId newNodeIds = node {nodeID = newNodeId, nodeIDs = newNodeIds}

increaseMsgID :: Node a -> Node a
increaseMsgID n = n {nextMsgID = nextMsgID n + 1}

decodeMessage :: BS.ByteString -> Message
decodeMessage = fromJust . decode
