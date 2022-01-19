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
    ) where


import Control.Concurrent.STM
import qualified Data.Text as DT
import qualified Data.Sequence as DS
import qualified Data.Map.Strict as DM
import Data.UUID.V4 (nextRandom)
import Data.UUID (toText)
import Network.WebSockets as WS

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


type ConnMap   = DM.Map UID WS.Connection
type InfoMap   = DM.Map UID UserInfo
type CursorMap = DM.Map UID CursorData


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


initState :: State
initState = State initOps initLang initCursors
  where
    initLang    = DT.pack "txt"
    initOps     = DS.fromList ([] :: [UserOperation])
    initCursors = DM.empty :: CursorMap


initSession :: UID -> STM (TVar HaskpadSession)
initSession uid = do
    let state = initState
        conns = DM.empty :: ConnMap
        infos = DM.empty :: InfoMap
    mutSess <- newTVar (HaskpadSession uid state conns infos False)   
    return mutSess


createSession :: IO (TVar HaskpadSession)
createSession = do
    uid <- getUID
    newSess <- atomically (initSession uid)
    return newSess 


addClientConn :: TVar HaskpadSession -> (UID, WS.Connection) -> STM ()
addClientConn sess newClient = do
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
        infoMap      = clientInfos currSess
        updatedMap   = DM.insert uid uinfo infoMap
    writeTVar sess (currSess {clientInfos = updatedMap})


deleteClientInfo :: TVar HaskpadSession -> UID -> STM ()
deleteClientInfo sess uid = do
    currSess <- readTVar sess
    let infoMap    = clientInfos currSess
        updatedMap = DM.delete uid infoMap
    writeTVar sess (currSess {clientInfos = updatedMap})


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

