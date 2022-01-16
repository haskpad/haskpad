{-
 - Implements data types representing a collaborative editing session.
-}
{-# LANGUAGE DuplicateRecordFields#-}

module Haskpad.Backend.Session 
    ( UID
    , HaskpadSession (..)
    , State (..)
    , UserOperation (..)
    , UserInfo (..)
    , CursorData (..)
    , History (..)
    ) where


import qualified Data.Map.Strict as DM
-- import qualified Network.WebSockets as WS
import Haskpad.Optra.Operation as OP


type UID         = Int
type Client      = Int -- (UID, WS.Connection)
type ServerState = Int --DM.Map UID WS.Connection


data HaskpadSession = HaskpadSession 
    { state         :: State
    , count         :: Int
    , clients       :: [Client]
    , serverState   :: ServerState
    , killed        :: Bool
    }


data State = State
    { operations :: [UserOperation]
    , text       :: String
    , language   :: String
    , users      :: DM.Map UID UserInfo
    , cursors    :: DM.Map UID CursorData
    } 


data UserOperation = UserOperation 
    { uid      :: UID
    , operation :: OP.OperationSeq
    }
 

data UserInfo = UserInfo
    { uid  :: UID
    , name  :: String
    , hue   :: Int
    }


data CursorData = CursorData
    { cursor     :: [Int]
    , selections :: [(Int, Int)]  
    }


data History = History 
    { start      :: Int
    , opHistoty  :: [UserOperation]
    }
