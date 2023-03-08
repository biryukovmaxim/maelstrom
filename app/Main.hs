{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}

import qualified Data.Aeson.Types as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Scientific
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import qualified Data.Vector as V
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

type MyState = [Int]

myNode :: Node MyState
myNode =
  Node
    { nodeID = "",
      nextMsgID = 0,
      nodeIDs = [],
      customHandler = myCustomHandler,
      state = []
    }

myCustomHandler ::  String -> MyState -> MessageBody -> IO (Maybe (MyState, MessageBody))
myCustomHandler mt = handler
  where 
    handler =
      case mt of
        "echo" -> echoHandler
        "generate" -> generateIDHandler
        "broadcast" -> broadcastHandler
        "read" -> readHandler
        "topology" -> topologyHandler
        _ -> \ _ _ -> pure Nothing
      

echoHandler :: Applicative f => a -> MessageBody -> f (Maybe (a, MessageBody))
echoHandler s inputBody = pure $ Just (s, inputBody {msgType = CustomMessageType "echo_ok"})

generateIDHandler :: a -> MessageBody -> IO (Maybe (a, MessageBody))
generateIDHandler s inputBody = do
  randomUUID <- nextRandom
  let uuidString = T.String $ toText randomUUID
  let oldOther = other inputBody
  let newOther = HM.insert "id" uuidString oldOther
  pure $ Just (s, inputBody {msgType = CustomMessageType "generate_ok", other = newOther})

broadcastHandler :: (Applicative f, Integral a, Bounded a) => [a] -> MessageBody -> f (Maybe ([a], MessageBody))
broadcastHandler s inputBody = pure $ Just (newS, outBody)
  where
    value = HM.lookup "message" $ other inputBody
    newS = case value of
      Just (T.Number n) -> s ++ [fromJust $ toBoundedInteger n]
      _ -> s
    outBody = inputBody {other = HM.empty, msgType = CustomMessageType "broadcast_ok"}

readHandler :: (Applicative f, Real a) => [a] -> MessageBody -> f (Maybe ([a], MessageBody))
readHandler s inputBody = pure $ Just (s, outBody)
  where
    messages = T.Array $ V.fromList $ map ((T.Number . unsafeFromRational) . toRational) s
    fields = HM.insert "messages" messages HM.empty
    outBody = inputBody {other = fields, msgType = CustomMessageType "read_ok"}

topologyHandler :: Applicative f => a -> MessageBody -> f (Maybe (a, MessageBody))
topologyHandler s inputBody = pure $ Just (s, outBody)
  where
    outBody = inputBody {other = HM.empty, msgType = CustomMessageType "topology_ok"}