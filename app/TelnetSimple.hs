module TelnetSimple (State, connect, send, recv, recvAll) where

import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Loops (whileJust_)
import Control.Monad.STM (atomically)
import Data.ByteString.Char8 qualified as B8
import Network.Simple.TCP qualified as TCP (HostName, Socket, connect, recv, send)
import Network.Telnet.LibTelnet qualified as Telnet

data State = State (STM.TBQueue B8.ByteString) (MVar ()) Telnet.Telnet

telnetH :: STM.TBQueue B8.ByteString -> TCP.Socket -> Telnet.EventHandler
telnetH readBuf _ _ (Telnet.Received b) = atomically $ STM.writeTBQueue readBuf b
telnetH _ s _ (Telnet.Send b) = TCP.send s b
telnetH _ _ _ _ = pure ()

handle :: State -> TCP.Socket -> IO ()
handle (State _ done telnet) s = do
  whileJust_ (TCP.recv s 4096) $ \bs -> do
    Telnet.telnetRecv telnet bs
  putMVar done ()

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

send :: State -> B8.ByteString -> IO ()
send (State _ _ telnet) = Telnet.telnetSend telnet

recv :: State -> IO B8.ByteString
recv (State readBuf _ _) = atomically . STM.readTBQueue $ readBuf

recvAll :: State -> IO B8.ByteString
recvAll (State readBuf _ _) = atomically $ mconcat <$> STM.flushTBQueue readBuf

connect :: TCP.HostName -> (State -> IO a) -> IO (Maybe a)
connect host f = do
  TCP.connect
    host
    "telnet"
    ( \(sock, _) -> do
        state@(State _ done _) <- do
          readBuf <- STM.newTBQueueIO 10
          done <- newEmptyMVar
          telnet <- Telnet.telnetInit [] [] (telnetH readBuf sock)
          pure $ State readBuf done telnet
        (_, r) <-
          concurrently
            (handle state sock)
            ( race
                (takeMVar done)
                (f state)
            )
        pure (rightToMaybe r)
    )
