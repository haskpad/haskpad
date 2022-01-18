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


import Control.Concurrent.STM (TVar, readTVar, writeTVar)
import qualified Data.Text as DT
import qualified Data.Sequence as DS
import qualified Data.Map.Strict as DM
import Data.UUID.V4 (nextRandom)
import Data.UUID (toText)
import qualified Network.Websockets as WS

import Haskpad.Optra.Operation as OP


type UID = DT.Text


getUID :: IO DT.Text
getUID = nextRandom >>= \u -> return $ toText u


data UserOperation = UserOperation
    { userID    :: DT.Text
    , operation :: OP.OperationSeq
    }


data UserInfo = UserInfo
    { userID :: DT.Text
    , name   :: String
    , hue    :: Int
    }


data CursorData = CursorData
    { cursor     :: [Int]
    , selections :: [(Int, Int)]  
    }


data History = History 
    { start      :: Int
    , opHistoty  :: DS.Seq UserOperation
    }


type ConnMap   = DM.Map UID WS.Connection
type InfoMap   = DM.Map UID UserInfo
type CursorMap = DM.Map UID CursorData


data HaskpadSession = HaskpadSession 
    { sessionID     :: UID
    , state         :: State
    , clientConns   :: TVar ConnMap
    , clientInfos   :: TVar InfoMap
    , killed        :: Bool
    }


data State = State
    { operations :: TVar (DS.Seq UserOperation)
    , text       :: TVar DT.Text
    , language   :: TVar DT.Text
    , cursors    :: TVar CursorMap
    }


addClientConn :: TVar ConnMap -> (UID, WS.Connection) -> STM ()


deleteClientConn :: TVar ConnMap -> UID -> STM ()


addClientInfo :: TVar InfoMap -> (UID, UserInfo) -> STM ()


deleteClientInfo :: TVar InfoMap -> UID -> STM ()


addOperation :: TVar (DS.Seq UserOperation) -> UserOperation -> STM ()


updateText :: TVar DT.Text -> [UserOperation] -> STM ()


updateLanguage :: TVar DT.Text -> DT.Text -> STM ()


updateCursors :: TVar CursorMap -> (UID, CursorData) -> STM ()


