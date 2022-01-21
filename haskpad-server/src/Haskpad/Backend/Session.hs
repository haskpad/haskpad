{-
 - Data types representing a collaborative editing session
   and related operations.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Haskpad.Backend.Session
  ( UID,
    getUID,
    UserOperation (..),
    UserInfo (..),
    CursorData (..),
    SessionMap,
    ConnMap,
    InfoMap,
    CursorMap,
    HaskpadSession (..),
    Document (..),
    emptyDocument,
    emptySessMap,
    createSession,
    addNewSession,
    addClientConn,
    deleteClientConn,
    addClientInfo,
    deleteClientInfo,
    addOperation,
    updateLanguage,
    printTVar,
    lookupSession,
    showSession,
  )
where

import Control.Concurrent.STM
import qualified Data.Map.Strict as DM
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Lazy as TL
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Haskpad.Optra.Operation as OP
import Network.WebSockets.Connection (Connection)

type UID = TL.Text

getUID :: IO UID
getUID = nextRandom >>= \u -> return $ (TL.fromStrict . toText) u

data UserOperation = UserOperation
  { userOperationId :: UID, -- this is just the userID
    userOperationOp :: OP.OperationSeq
  }
  deriving (Show)

data UserInfo = UserInfo
  { userInfoName :: TL.Text,
    userInfoHue :: TL.Text
  }
  deriving (Show, Eq)

data CursorData = CursorData
  { cursorDataCursor :: [Int],
    cursorDataSelections :: [(Int, Int)]
  }
  deriving (Show)

type SessionMap = DM.Map UID (TVar HaskpadSession)

type ConnMap = DM.Map UID Connection

type InfoMap = DM.Map UID UserInfo

type CursorMap = DM.Map UID CursorData

data HaskpadSession = HaskpadSession
  { haskpadSessionId :: UID,
    haskpadSessionDocument :: Document,
    haskpadSessionClientConns :: ConnMap,
    haskpadSessionClientInfos :: InfoMap,
    haskpadSessionKilled :: Bool
  }

data Document = Document
  { documentUserOps :: DS.Seq UserOperation,
    documentLanguage :: TL.Text,
    documentCursors :: CursorMap
  }
  deriving (Show)

-- | Initialize an empty state.
emptyDocument :: Document
emptyDocument = Document initOps initLang initCursors
  where
    initLang = TL.pack "txt"
    initOps = DS.fromList ([] :: [UserOperation])
    initCursors = DM.empty :: CursorMap

-- | Initialize an empty mutable session given the session id.
emptySession :: UID -> STM (TVar HaskpadSession)
emptySession uid = do
  let doc = emptyDocument
      conns = DM.empty :: ConnMap
      infos = DM.empty :: InfoMap
  mutSess <- newTVar (HaskpadSession uid doc conns infos False)
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
addClientConn :: TVar HaskpadSession -> (UID, Connection) -> STM ()
addClientConn sess newClient = do
  currSess <- readTVar sess
  let (uid, conn) = newClient
      connMap = haskpadSessionClientConns currSess
      updatedMap = DM.insert uid conn connMap
  writeTVar sess (currSess {haskpadSessionClientConns = updatedMap})

-- | Delete a client connction from an active HaskpadSession.
deleteClientConn :: TVar HaskpadSession -> UID -> STM ()
deleteClientConn sess uid = do
  currSess <- readTVar sess
  let connMap = haskpadSessionClientConns currSess
      updatedMap = DM.delete uid connMap
  writeTVar sess (currSess {haskpadSessionClientConns = updatedMap})

-- | Add client info identified by a uid to an active HaskpadSession.
addClientInfo :: TVar HaskpadSession -> (UID, UserInfo) -> STM ()
addClientInfo sess clientInfo = do
  currSess <- readTVar sess
  let (uid, uinfo) = clientInfo
      infoMap = haskpadSessionClientInfos currSess
      updatedMap = DM.insert uid uinfo infoMap
  writeTVar sess (currSess {haskpadSessionClientInfos = updatedMap})

-- | Delete client info indentified by a uid to an active HaskpadSession.
deleteClientInfo :: TVar HaskpadSession -> UID -> STM ()
deleteClientInfo sess uid = do
  currSess <- readTVar sess
  let infoMap = haskpadSessionClientInfos currSess
      updatedMap = DM.delete uid infoMap
  writeTVar sess (currSess {haskpadSessionClientInfos = updatedMap})

-- | Add an operation to the UserOperation sequence in an activate HaskpadSession.
addOperation :: TVar HaskpadSession -> UserOperation -> STM ()
addOperation sess op = do
  currSess <- readTVar sess
  let currDoc = haskpadSessionDocument currSess
      ops = documentUserOps currDoc
      updatedOps = ops DS.|> op
      updatedDoc = currDoc {documentUserOps = updatedOps}
  writeTVar sess (currSess {haskpadSessionDocument = updatedDoc})

-- | Add an operation to the UserOperation sequence in an activate HaskpadSession.
updateLanguage :: TVar HaskpadSession -> TL.Text -> STM ()
updateLanguage sess lang = do
  currSess <- readTVar sess
  let currDoc = haskpadSessionDocument currSess
      updatedDoc = currDoc {documentLanguage = lang}
  writeTVar sess (currSess {haskpadSessionDocument = updatedDoc})

-- | Print a TVar for debugging
printTVar :: Show a => TVar a -> IO ()
printTVar t = do
  tr <- atomically $ readTVar t
  putStrLn (show tr)

-- | Print TVar Session because it has no instance of show.
showSession :: TVar HaskpadSession -> IO String
showSession sess = do
  s <- atomically $ readTVar sess
  let doc = show (haskpadSessionDocument s)
  let clientInfos = show (haskpadSessionClientInfos s)
  return $ doc ++ "\n" ++ clientInfos

-- | Lookup and print a session from a TVar SessionMap, for debugging.
lookupSession :: TVar SessionMap -> TL.Text -> IO ()
lookupSession sm sid = do
  sm' <- atomically $ readTVar sm
  let sess = DM.lookup sid sm'
  case sess of
    Nothing -> putStrLn "Session not found"
    Just s -> do
      st <- showSession s
      putStrLn (st)
