{-# LANGUAGE BlockArguments #-}

import qualified Data.ByteString.Lazy as BS
import Node (emptyNode, handleRawMessage)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as BC

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
  input <- BS.getContents
  let result = $! handleRawMessage emptyNode  input
    in putStrLn $! BC.unpack $ snd (fromJust result)
  
  