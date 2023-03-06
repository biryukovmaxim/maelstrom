{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}

import Node (emptyNode, handleRawMessage, Node)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB

import System.IO (hSetBuffering, stdin, BufferMode(..), hFlush, stdout)

-- main :: IO ()
-- main = loop emptyNode
--  where
--    loop node = do
--      input <- BS.getContents
--      case handleRawMessage node input of
--        Just (updatedNode, output) -> do
--          BS.putStr output
--          loop updatedNode
--        Nothing -> loop node

main :: IO ()

main = do
  hSetBuffering stdin LineBuffering
  loop emptyNode

loop :: Node -> IO ()
loop node = do
  input <- BS.hGetLine stdin
  let lazyInput = BSL.fromStrict input
  let nodeAndMsg = handleRawMessage node lazyInput
  case nodeAndMsg of
    Just (newNode, outPutMsg) -> do
      BSL.putStr $ appendNewline outPutMsg
      hFlush stdout
      loop newNode
    Nothing -> loop node
      

appendNewline :: BSL.ByteString -> BSL.ByteString
appendNewline bs = BSB.toLazyByteString $ BSB.lazyByteString bs <> BSB.charUtf8 '\n'
