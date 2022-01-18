{-
 - Implements data types representing a collaborative editing session.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

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
    { name   :: String
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
    , clientConns   :: ConnMap
    , clientInfos   :: InfoMap
    , killed        :: Bool
    } deriving (Show)


data State = State
    { operations :: DS.Seq UserOperation
    , text       :: DT.Text
    , language   :: DT.Text
    , cursors    :: CursorMap
    } deriving (Show)


addClientConn :: TVar HaskpadSession -> (UID, WS.Connection) -> STM ()
addClientConn sess{..} newClient{..} = do
    currSess <- readTVar sess
    let (uid, conn) = newClient
        connMap     = clientConns currSess
        updatedMap  = DM.insert uid conn connMap
    writeTVar sess (currSess {clientConns = updatedMap}) 


deleteClientConn :: TVar HaskpadSession -> UID -> STM ()
deleteClientConn sess uid = do
    currSess <- readTVar sess
    let connMap    = clientConns currSess
        updatedMap = DM.delete uid connMap
    writeTVar sess (currSess {clientConns = updatedMap})


addClientInfo :: TVar HaskpadSession -> (UID, UserInfo) -> STM ()
addClientInfo sess clientInfo = do
    currSess <- readTVar sess
    let (uid, uinfo) = clientInfo
        infoMap      = clientInfos sess
        updatedMap   = DM.insert uid conn infoMap
    writeTVar sess (currSess {clientInfos = updatedMap})


deleteClientInfo :: TVar HaskpadSession -> UID -> STM ()
    currSess <- readTVar sess
    let infoMap    = clientInfos currSess
        updatedMap = DM.delete uid infoMap
    writeTVar sess (currSess {clientInfos = updatedMap})


addOperation :: TVar HaskpadSession -> UserOperation -> STM ()
addOperation sess op = do
    currSess <- readTVar sess
    let currState    = state currSess
        ops          = operations currState
        updatedOps   = ops DS.|> op
        updatedState = currState {operations = updatedOps}
     writeTVar sess (currSess {state = updatedState})


updateLanguage :: TVar HaskpadSession -> DT.Text -> STM ()
updateLanguage sess newLang = do
    currSess <- readTVar sess
    let currState   = state currSess
        updatedLang = currState {language = newLang}
    writeTvar sess (currSess {state=updatedLang})


updateCursors :: TVar HaskpadSession -> (UID, CursorData) -> STM ()
updatedLanguage sess clientCursor = do
    currSess <- readTVar sess
    let (uid, cursor)  = clientCursor
        currState      = state currSess 
        cursorMap      = cursors currState 
        updatedCursors = DM.insert uid cursor cursorMap
        updatedState   = currState {cursors = updatedCursors}
    writeTVar sess (currSess {state = updatedState})


