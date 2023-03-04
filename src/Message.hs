{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Message
  ( Message (..),
    MessageBody (..),
    MessageType (..),
  )
where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import EncodeOptions(msgBodyOptions)
import Data.Aeson.TH (deriveJSON)

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
    nodeIds :: Maybe [String]
  }
  deriving (Show, Eq, Generic)

data MessageType = InitMessageType | CustomMessageType String | InitOkType
  deriving (Show, Eq)

--instance FromJSON MessageBody where
--  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

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

$(deriveJSON msgBodyOptions ''MessageBody)
instance ToJSON Message
instance FromJSON Message