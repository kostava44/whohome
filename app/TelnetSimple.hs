module TelnetSimple (connect) where

import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Loops (whileJust_)
import Control.Monad.STM (atomically)
import Data.ByteString.Char8 qualified as B8
import Network.Simple.TCP qualified as TCP (HostName, Socket, connect, recv, send)
import Network.Telnet.LibTelnet qualified as Telnet

data State = State (STM.TBQueue B8.ByteString) (MVar ())

telnetH :: State -> TCP.Socket -> Telnet.EventHandler
telnetH (State readBuf _) _ _ (Telnet.Received b) = atomically $ STM.writeTBQueue readBuf b
telnetH _ s _ (Telnet.Send b) = TCP.send s b
telnetH _ _ _ _ = pure ()

handle :: State -> Telnet.Telnet -> TCP.Socket -> IO ()
handle (State _ done) telnet s = do
  whileJust_ (TCP.recv s 4096) $ \bs -> do
    Telnet.telnetRecv telnet bs
  putMVar done ()

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

connect :: TCP.HostName -> ((B8.ByteString -> IO (), IO B8.ByteString) -> IO a) -> IO (Maybe a)
connect host f = do
  readBuf <- STM.newTBQueueIO 10
  done <- newEmptyMVar
  let state = State readBuf done

  TCP.connect
    host
    "telnet"
    ( \(sock, _) -> do
        telnet <- Telnet.telnetInit [] [] (telnetH state sock)
        (_, r) <-
          concurrently
            (handle state telnet sock)
            ( race
                (takeMVar done)
                ( let send = Telnet.telnetSend telnet
                      recv = atomically . STM.readTBQueue $ readBuf
                   in f (send, recv)
                )
            )
        pure (rightToMaybe r)
    )
