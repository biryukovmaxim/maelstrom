{-# LANGUAGE BlockArguments #-}

import Control.Monad (forever)
import qualified Data.ByteString.Lazy as BS
import Node

main :: IO ()
main = forever $ do
  input <- BS.getContents
  BS.putStr input
  let maybeResult = handleRawMessage input
   in case maybeResult of
        Just (_, output) -> BS.putStr output
        Nothing -> return ()
