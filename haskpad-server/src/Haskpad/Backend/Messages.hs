{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Haskpad.Backend.Messages
    ( ServerMessage (..)
    , ClientMessage (..)
    ) where


import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Text.Lazy as DT
import qualified Data.Text.Lazy.Encoding as DTE 
import qualified Data.Foldable as DF

import Haskpad.Backend.Session as BS
import Haskpad.Optra.Serialize as OPS


data ServerMessage
    = IdentityMsg Int
    | HistoryMsg Int [(Int, DT.Text)]
    | LanguageMsg DT.Text
    | UserInfoMsg Int DT.Text
    | UserCursorMsg Int DT.Text
    deriving (Generic)


instance FromJSON ServerMessage where
    parseJSON = withObject "ServerMessage" $ \v -> DF.asum
      [ IdentityMsg <$> v .: "socket_id"
      , HistoryMsg <$> v .: "start" <*> v .: "operations"
      , LanguageMsg <$> v .: "current_language"
      , UserInfoMsg <$> v .: "uid" <*> v .: "user_info"
      , UserCursorMsg <$> v .: "uid" <*> v .: "cursor_data"
      ]


instance ToJSON ServerMessage where
    toEncoding = genericToEncoding defaultOptions


{-
toHistoryMsg :: BS.History -> ServerMessage
toHistoryMsg (BS.History start userOps) = HistoryMsg start (serUserOps userOps)
  where
    serUserOps = map (\(BS.UserOperation uid ops) -> (uid, parseOps ops))
    parseOps   = DTE.decodeUtf8 . OPS.serializeOps
-}



data ClientMessage 
    = EditMsg Int [DT.Text]
    | SetLanguageMsg DT.Text
    | ClientInfoMsg DT.Text DT.Text
    | CursorDataMsg [Int] [(Int, Int)]
    deriving (Generic) 


instance FromJSON ClientMessage where
    parseJSON = withObject "ClientMessage" $ \v -> DF.asum
      [ EditMsg <$> v .: "revision" <*> v .: "operations"  
      , SetLanguageMsg <$> v .: "set_language"
      , ClientInfoMsg <$> v .: "name" <*> v .: "hue"
      , CursorDataMsg <$> v .: "cursors" <*> v .: "selections" 
      ]


instance ToJSON ClientMessage where
    toEncoding = genericToEncoding defaultOptions



