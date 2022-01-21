{-
 - Message types for Client and Server communication.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskpad.Backend.Message
  ( ServerMessage (..),
    ClientMessage (..),
  )
where

import Data.Aeson
import qualified Data.Foldable as DF
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Haskpad.Backend.Session as BS
import Haskpad.Optra.Serialize as OPS

data ServerMessage
  = IdentityMsg Int
  | HistoryMsg Int [(Int, TL.Text)]
  | LanguageMsg TL.Text
  | UserInfoMsg Int TL.Text
  | UserCursorMsg Int TL.Text
  deriving (Generic)

instance FromJSON ServerMessage where
  parseJSON = withObject "ServerMessage" $ \v ->
    DF.asum
      [ IdentityMsg <$> v .: "socket_id",
        HistoryMsg <$> v .: "start" <*> v .: "operations",
        LanguageMsg <$> v .: "current_language",
        UserInfoMsg <$> v .: "uid" <*> v .: "user_info",
        UserCursorMsg <$> v .: "uid" <*> v .: "cursor_data"
      ]

instance ToJSON ServerMessage where
  toEncoding = genericToEncoding defaultOptions

data ClientMessage
  = EditMsg TL.Text TL.Text Int [TL.Text]
  | SetLanguageMsg TL.Text TL.Text TL.Text
  | ClientInfoMsg TL.Text TL.Text TL.Text TL.Text
  | CursorDataMsg TL.Text TL.Text [Int] [(Int, Int)]
  | ClientErrorMsg
  deriving (Generic)

instance FromJSON ClientMessage where
  parseJSON = withObject "ClientMessage" $ \v ->
    DF.asum
      [ EditMsg
          <$> v .: "sessId"
          <*> v .: "uid"
          <*> v .: "revision"
          <*> v .: "operations",
        SetLanguageMsg
          <$> v .: "sessId"
          <*> v .: "uid"
          <*> v .: "set_language",
        ClientInfoMsg
          <$> v .: "sessId"
          <*> v .: "uid"
          <*> v .: "name"
          <*> v .: "hue",
        CursorDataMsg
          <$> v .: "sessId"
          <*> v .: "uid"
          <*> v .: "cursors"
          <*> v .: "selections"
      ]

instance ToJSON ClientMessage where
  toEncoding = genericToEncoding defaultOptions

{-
toHistoryMsg :: BS.History -> ServerMessage
toHistoryMsg (BS.History start userOps) = HistoryMsg start (serUserOps userOps)
  where
    serUserOps = map (\(BS.UserOperation uid ops) -> (uid, parseOps ops))
    parseOps   = DTE.decodeUtf8 . OPS.serializeOps
-}
