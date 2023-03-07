{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}

module Message
  ( Message (..),
    MessageBody (..),
    MessageType (..),
  )
where

import Data.Aeson
import Data.Aeson.Key (fromString)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import GHC.Generics

data Message = Message
  { src :: String,
    dest :: String,
    body :: MessageBody
  }
  deriving (Show, Eq, Generic)

data MessageBody = MessageBody
  { msgType :: MessageType,
    msgId :: Maybe Int,
    inReplyTo :: Maybe Int,
    code :: Maybe Int,
    text :: Maybe String,
    nodeId :: Maybe String,
    nodeIds :: Maybe [String],
    other :: HM.HashMap String Value -- catch-all field
  }
  deriving (Show, Eq, Generic)

data MessageType = InitMessageType | CustomMessageType String | InitOkType
  deriving (Show, Eq)

instance FromJSON MessageType where
  parseJSON (String s)
    | s == T.pack "init" = pure InitMessageType
    | s == T.pack "init_ok" = pure InitMessageType
    | otherwise = pure $ CustomMessageType (T.unpack s)
  parseJSON _ = fail "Invalid MessageType"

instance ToJSON MessageType where
  toJSON InitMessageType = String $ T.pack "init"
  toJSON InitOkType = String $ T.pack "init_ok"
  toJSON (CustomMessageType s) = String (T.pack s)

instance FromJSON MessageBody where
  parseJSON = withObject "MessageBody" $ \o -> do
    let knownFields = ["type", "msg_id", "in_reply_to", "code", "text", "node_id", "node_ids"]
    unknownFields <- flip HM.filterWithKey <$> parseJSON (Object o) <*> pure (\key _ -> key `notElem` knownFields)
    msgType <- o .: fromString "type"
    msgId <- o .:? fromString "msg_id"
    inReplyTo <- o .:? fromString "in_reply_to"
    code <- o .:? fromString "code"
    text <- o .:? fromString "text"
    nodeId <- o .:? fromString "node_id"
    nodeIds <- o .:? fromString "node_ids"
    return $ MessageBody msgType msgId inReplyTo code text nodeId nodeIds unknownFields

instance ToJSON MessageBody where
  toJSON (MessageBody msgType msgId inReplyTo code text nodeId nodeIds other) =
    let knownFields =
          [ Just $ fromString "type" .= msgType,
            fromString "msg_id" .=? msgId,
            fromString "in_reply_to" .=? inReplyTo,
            fromString "code" .=? code,
            fromString "text" .=? text,
            fromString "node_id" .=? nodeId,
            fromString "node_ids" .=? nodeIds
          ]
        unknownFields = HM.toList other
        allFields = catMaybes knownFields ++ map (\(k, v) -> fromString k .= v) unknownFields
     in object allFields

instance FromJSON Message

instance ToJSON Message

-- Helper function to encode optional fields
(.=?) ::
  ToJSON a =>
  Key ->
  Maybe a ->
  Maybe (Key, Value)
(.=?) _ Nothing = Nothing
(.=?) k (Just v) = Just (k .= v)
