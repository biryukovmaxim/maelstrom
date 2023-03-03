{-# LANGUAGE DeriveGeneric #-}

module Message
  ( Message,
    MessageBody,
  )
where

import Data.Aeson
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
  { messageType :: Maybe String,
    msgID :: Maybe Int,
    inReplyTo :: Maybe Int,
    code :: Maybe Int,
    text :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance FromJSON MessageBody

instance ToJSON MessageBody
