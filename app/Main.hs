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

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  loop myNode

loop :: Node MyState -> IO ()
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

type MyState = ()

myNode :: Node MyState
myNode =
  Node
    { nodeID = "",
      nextMsgID = 0,
      nodeIDs = [],
      customHandler = myCustomHandler,
      state = ()
    }

myCustomHandler :: MyState -> String -> MessageBody -> IO (Maybe (MyState, MessageBody))
myCustomHandler s "echo" inputBody = pure $ Just (s, inputBody {msgType = CustomMessageType "echo_ok"})
myCustomHandler _ _ _ = pure Nothing
