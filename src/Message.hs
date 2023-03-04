{-# LANGUAGE DeriveGeneric #-}

module Message
  ( Message (..),
    MessageBody (..),
    MessageType (..),
  )
where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Message = Message
  { src :: Maybe String,
    dest :: Maybe String,
    body :: Maybe MessageBody
  }
  deriving (Show, Generic)

instance FromJSON Message

instance ToJSON Message

data MessageBody = MessageBody
  { messageType :: MessageType,
    msgID :: Maybe Int,
    inReplyTo :: Maybe Int,
    code :: Maybe Int,
    text :: Maybe String,
    nodeId :: Maybe String,
    nodeIds :: Maybe [String]
  }
  deriving (Show, Eq, Generic)

instance FromJSON MessageBody

instance ToJSON MessageBody

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
