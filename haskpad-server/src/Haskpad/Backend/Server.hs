module Haskpad.Backend.Server
    (
    ) where

import           Control.Concurrent.STM (STM, TVar)
import           Control.Exception (handle)
import           Control.Monad (forM_)
import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as DT
import qualified Data.Text.Lazy as TL
import qualified Network.WebSockets as WS
import           Network.WebSockets.Connection (Connection)

import Haskpad.Backend.Session as BS
import Haskpad.Backend.Message as BM


-- | Handle connection exceptions during client communication
connExcHandler :: WS.ConnectionException -> IO ()
connExcHandler (WS.CloseRequest _ _) = putStrLn "Client closed the connection"
connExcHandler WS.ConnectionClosed   = putStrLn "Connection closed unexpectedly"
connExcHandler _                     = putStrLn "Connection closed, reason unknown"


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
        _        -> BM.ClientErrorMsg


-- | Serve clients
haskpadServe :: Connection -> IO ()
haskpadServe conn = handle connExcHandler $ do
    bytes <- WS.receiveData conn :: IO BL.ByteString
    let msg = parseMessage bytes
    case msg of
      (BM.EditMsg sid uid rev ops)         -> handleEdit sid uid rev ops
      (BM.SetLanguageMsg sid uid lang)     -> handleSetLang sid uid lang
      (BM.ClientInfoMsg sid uid name hue)  -> handleInfo sid uid name hue
      (BM.CursorDataMsg sid uid curs sels) -> handleCursor sid uid curs sels 
      BM.ClientErrorMsg                    -> putStrLn "Failed to parse msg"
    haskpadServe conn


handleEdit :: BS.UID -> BS.UID -> Int -> [TL.Text] -> IO ()
handleEdit _ _ _ _ = putStrLn "to be implemented"


handleSetLang :: BS.UID -> BS.UID -> TL.Text -> IO ()
handleSetLang _ _ _ = putStrLn "to be implemented"


handleInfo :: BS.UID -> BS.UID -> TL.Text -> TL.Text -> IO ()
handleInfo _ _ _ _ = putStrLn "to be implemented"


handleCursor :: BS.UID -> BS.UID -> [Int] -> [(Int, Int)] -> IO ()
handleCursor _ _ _ _ = putStrLn "to be implemented" 


