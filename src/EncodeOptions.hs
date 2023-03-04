{-# LANGUAGE LambdaCase #-}

module EncodeOptions
  ( msgBodyOptions,
  )
where

import Data.Aeson
import Data.Aeson.TH()

msgBodyOptions :: Options
msgBodyOptions =
  defaultOptions
    { fieldLabelModifier =
        \case
          "msgType" -> "type"
          f -> camelTo2 '_' f
    , omitNothingFields = True
    }