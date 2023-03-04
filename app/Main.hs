{-# LANGUAGE BlockArguments #-}

import qualified Data.ByteString.Lazy as BS
import Node (handleRawMessage, emptyNode)

main :: IO ()
main = loop emptyNode
  where
    loop node = do
      input <- BS.getContents
      case handleRawMessage node input of
        Just (updatedNode, output) -> do
          BS.putStr output
          loop updatedNode
        Nothing -> loop node
