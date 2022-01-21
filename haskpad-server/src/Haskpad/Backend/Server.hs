module Haskpad.Backend.Server
  ( handleSetLang,
  )
where

import Control.Concurrent.STM
import Control.Exception (handle)
import Control.Monad (forM_)
import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as DM
import qualified Data.Text as DT
import qualified Data.Text.Lazy as TL
import Haskpad.Backend.Message as BM
import Haskpad.Backend.Session as BS
import qualified Network.WebSockets as WS
import Network.WebSockets.Connection (Connection)

-- | Handle connection exceptions during client communication
connExcHandler :: WS.ConnectionException -> IO ()
connExcHandler (WS.CloseRequest _ _) = putStrLn "Client closed the connection"
connExcHandler WS.ConnectionClosed = putStrLn "Connection closed unexpectedly"
connExcHandler _ = putStrLn "Connection closed, reason unknown"

-- | Send message to a single client.
sendMessage :: Connection -> BL.ByteString -> IO ()
sendMessage conn msg = handle connExcHandler $ WS.sendTextData conn msg

-- | Send message to a group of clients.
broadcastMessage :: [(BS.UID, Connection)] -> BL.ByteString -> IO ()
broadcastMessage conns msg = do
  forM_ conns $ \(_, conn) -> sendMessage conn msg

-- | Parse raw bytestring message to ClientMessage.
parseMessage :: BL.ByteString -> BM.ClientMessage
parseMessage bytes = do
  let decoded = DA.decode bytes :: Maybe BM.ClientMessage
  case decoded of
    Just msg -> msg
    _ -> BM.ClientErrorMsg

{-
-- | Serve clients
haskpadServe :: TVar BS.SessionMap -> Connection -> IO ()
haskpadServe ss conn = handle connExcHandler $ do
    bytes <- WS.receiveData conn :: IO BL.ByteString
    let msg = parseMessage bytes
    case msg of
      (BM.EditMsg sid uid rev ops)         -> handleEdit ss sid uid rev ops
      (BM.SetLanguageMsg sid uid lang)     -> do
          ok <- atomically $ handleSetLang ss sid uid lang
          putStrLn "wtf"
      (BM.ClientInfoMsg sid uid name hue)  -> handleInfo ss sid uid name hue
      (BM.CursorDataMsg sid uid curs sels) -> handleCursor ss sid uid curs sels
      BM.ClientErrorMsg                    -> putStrLn "Failed to parse msg"
    haskpadServe ss conn
-}

handleEdit ::
  TVar BS.SessionMap ->
  BS.UID ->
  BS.UID ->
  Int ->
  [TL.Text] ->
  IO ()
handleEdit _ _ _ _ _ = putStrLn "to be implemented"

handleSetLang ::
  TVar BS.SessionMap ->
  BS.UID ->
  BS.UID ->
  TL.Text ->
  STM ()
handleSetLang sessMap sid uid lang = do
  sessions <- readTVar sessMap
  let sess = DM.lookup sid sessions
  case sess of
    Nothing -> writeTVar sessMap sessions
    Just s -> do
      BS.updateLanguage s lang
      writeTVar sessMap (DM.insert sid s sessions)

handleClientInfo ::
  TVar BS.SessionMap ->
  BS.UID ->
  BS.UID ->
  TL.Text ->
  TL.Text ->
  STM ()
handleClientInfo sessMap sid uid name hue = do
  sessions <- readTVar sessMap
  let sess = DM.lookup sid sessions
  case sess of
    Nothing -> writeTVar sessMap sessions
    Just s -> do
      let usrInfo = (uid, BS.UserInfo name hue)
      BS.addClientInfo s usrInfo
      writeTVar sessMap (DM.insert sid s sessions)

handleCursor ::
  TVar BS.SessionMap ->
  BS.UID ->
  BS.UID ->
  [Int] ->
  [(Int, Int)] ->
  STM ()
handleCursor sessMap sid uid curs sels = do
  sessions <- readTVar sessMap
  let sess = DM.lookup sid sessions
  case sess of
    Nothing -> writeTVar sessMap sessions
    Just s -> do
      let cursInfo = (uid, BS.CursorData curs sels)
      BS.updateCursor s cursInfo
      writeTVar sessMap (DM.insert sid s sessions)
