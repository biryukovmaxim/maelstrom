module Node
  ( handleRawMessage,
  )
where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BS
import Message (Message)

data Node = Node
  { id :: String,
    nextMsgID :: Int,
    nodeIDs :: [String]
  }
  deriving (Show)

handleRawMessage :: BS.ByteString -> Maybe (Node, BS.ByteString)
handleRawMessage input = do
  message <- decode input
  Just (Node {}, encode $ handleMessage message)

handleMessage :: Message -> Message
handleMessage msg = msg
