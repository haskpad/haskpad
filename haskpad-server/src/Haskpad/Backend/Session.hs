{-
 - Data types representing a collaborative editing session
   and related operations.
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
    ) where


import           Control.Concurrent.STM
import qualified Data.Map.Strict as DM
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import           Data.UUID.V4 (nextRandom)
import           Data.UUID (toText)
import qualified Network.WebSockets as WS

import Haskpad.Optra.Operation as OP


type UID = DT.Text


getUID :: IO DT.Text
getUID = nextRandom >>= \u -> return $ toText u


data UserOperation = UserOperation
    { userID    :: DT.Text
    , operation :: OP.OperationSeq
    } deriving (Show) 


data UserInfo = UserInfo
    { name   :: String
    , hue    :: Int
    } deriving (Show, Eq)


data CursorData = CursorData
    { cursor     :: [Int]
    , selections :: [(Int, Int)]  
    } deriving (Show)


type SessionMap = DM.Map UID (TVar HaskpadSession)
type ConnMap    = DM.Map UID WS.Connection
type InfoMap    = DM.Map UID UserInfo
type CursorMap  = DM.Map UID CursorData


data HaskpadSession = HaskpadSession 
    { sessionID     :: UID
    , state         :: State
    , clientConns   :: ConnMap
    , clientInfos   :: InfoMap
    , killed        :: Bool
    }


data State = State
    { userOps    :: DS.Seq UserOperation
    , language   :: DT.Text
    , cursors    :: CursorMap
    } deriving (Show)


-- | Initialize an empty state. 
emptyState :: State
emptyState = State initOps initLang initCursors
  where
    initLang    = DT.pack "txt"
    initOps     = DS.fromList ([] :: [UserOperation])
    initCursors = DM.empty :: CursorMap


-- | Initialize an empty mutable session given the session id.
emptySession :: UID -> STM (TVar HaskpadSession)
emptySession uid = do
    let state = emptyState
        conns = DM.empty :: ConnMap
        infos = DM.empty :: InfoMap
    mutSess <- newTVar (HaskpadSession uid state conns infos False)   
    return mutSess


-- | Create a new TVar HaskpadSession. We have this because STM () does not
--   permit IO action inside it, in `emptySession`.
createSession :: IO (UID, TVar HaskpadSession)
createSession = do
    uid <- getUID
    newSess <- atomically (emptySession uid)
    return (uid, newSess) 


-- | Init an empty session map.
emptySessMap :: STM (TVar SessionMap)
emptySessMap = do
   sessMap <- newTVar (DM.empty :: SessionMap) 
   return sessMap


-- | Add a new empty session to a sessMap.
addNewSession :: TVar SessionMap -> UID -> TVar HaskpadSession -> STM ()
addNewSession sessMap sessId newSession = do
    sessMap' <- readTVar sessMap 
    writeTVar sessMap (DM.insert sessId newSession sessMap') 


-- | Add a websocket connection identified by UID to client connections.
addClientConn :: TVar HaskpadSession -> (UID, WS.Connection) -> STM ()
addClientConn sess newClient = do
    currSess <- readTVar sess
    let (uid, conn) = newClient
        connMap     = clientConns currSess
        updatedMap  = DM.insert uid conn connMap
    writeTVar sess (currSess {clientConns = updatedMap}) 


-- | Delete a client connction from an active HaskpadSession.
deleteClientConn :: TVar HaskpadSession -> UID -> STM ()
deleteClientConn sess uid = do
    currSess <- readTVar sess
    let connMap    = clientConns currSess
        updatedMap = DM.delete uid connMap
    writeTVar sess (currSess {clientConns = updatedMap})


-- | Add client info identified by a uid to an active HaskpadSession.
addClientInfo :: TVar HaskpadSession -> (UID, UserInfo) -> STM ()
addClientInfo sess clientInfo = do
    currSess <- readTVar sess
    let (uid, uinfo) = clientInfo
        infoMap      = clientInfos currSess
        updatedMap   = DM.insert uid uinfo infoMap
    writeTVar sess (currSess {clientInfos = updatedMap})


-- | Delete client info indentified by a uid to an active HaskpadSession.
deleteClientInfo :: TVar HaskpadSession -> UID -> STM ()
deleteClientInfo sess uid = do
    currSess <- readTVar sess
    let infoMap    = clientInfos currSess
        updatedMap = DM.delete uid infoMap
    writeTVar sess (currSess {clientInfos = updatedMap})


-- | Add an operation to the UserOperation sequence in an activate HaskpadSession.
addOperation :: TVar HaskpadSession -> UserOperation -> STM ()
addOperation sess op = do
    currSess <- readTVar sess
    let currState    = state currSess
        ops          = userOps currState
        updatedOps   = ops DS.|> op
        updatedState = currState {userOps = updatedOps}
    writeTVar sess (currSess {state = updatedState})


-- | Print a TVar for debugging
printTVar :: Show a => TVar a -> IO ()  
printTVar t = do
    tr <- atomically $ readTVar t
    putStrLn (show tr)

