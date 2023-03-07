{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Message
import Node (Node (..), handleRawMessage)
import System.IO
  ( BufferMode (..),
    hFlush,
    hSetBuffering,
    stdin,
    stdout,
  )
import Data.UUID.V4 (nextRandom)
import Data.UUID(toText)
import Data.HashMap.Strict (insert)
import qualified Data.Aeson.Types as T

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  loop myNode

loop :: Node -> IO ()
loop node = do
  input <- BS.hGetLine stdin
  let lazyInput = BSL.fromStrict input
  nodeAndMsg <- handleRawMessage node lazyInput
  case nodeAndMsg of
    Just (newNode, outPutMsg) -> do
      BSL.putStr $ appendNewline outPutMsg
      hFlush stdout
      loop newNode
    Nothing -> loop node

appendNewline :: BSL.ByteString -> BSL.ByteString
appendNewline bs = BSB.toLazyByteString $ BSB.lazyByteString bs <> BSB.charUtf8 '\n'

myNode :: Node
myNode =
  Node
    { nodeID = "",
      nextMsgID = 0,
      nodeIDs = [],
      customHandler = myCustomHandler
    }

myCustomHandler :: String -> MessageBody -> IO (Maybe MessageBody)
myCustomHandler "echo" inputBody = pure $ Just inputBody {inReplyTo = msgId inputBody, msgType = CustomMessageType "echo_ok"}
myCustomHandler "generate" inputBody = do
   randomUUID <- nextRandom
   let uuidString =  T.String $ toText randomUUID
   let oldOther = other inputBody
   let newOther = insert "id" uuidString oldOther
   pure $ Just inputBody {inReplyTo = msgId inputBody, msgType = CustomMessageType "generate_ok", other = newOther}

myCustomHandler _ _ = pure Nothing
