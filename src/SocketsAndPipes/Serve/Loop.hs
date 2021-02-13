module SocketsAndPipes.Serve.Loop ( run ) where

import SocketsAndPipes.Serve.ForkBracket

import Control.Monad  ( forever )
import Network.Socket ( Socket )

import qualified Network.Socket as Socket

run ::
    (Socket -> IO a) -- ^ What to do when a new client connects.
    -> Socket -- ^ A passive socket that is listening for connections.
    -> IO b
run server s = forever (acceptAndFork s server)
{- ^
    Perpetually awaits new connections,
    forking a new thread to handle each one.
-}

acceptAndFork ::
    Socket -- ^ A passive socket that is listening for connections.
    -> (Socket -> IO a) -- ^ What to do when a new client connects.
    -> IO ThreadId
acceptAndFork s = forkBracket (accept s) socketForkBracketCleanup
{- ^
    Waits until a new client shows up to connect to our server.
    When a peer connects, the socket for talking to them will
    be passed to the given function.
-}

socketForkBracketCleanup :: Cleanup Socket
socketForkBracketCleanup = Cleanup{onForkFail, onThreadEnd}
  where
    onThreadEnd = -- At the end of the thread:
        fin           -- Politely conclude the connection.

    onForkFail =  -- If an exception occurs before the thread even starts:
        Socket.close  -- Just close the socket abruptly.
                      -- Since this happens on the main thread, we don't
                      -- want to take the time to wait for a graceful close.

accept ::
    Socket -- ^ A passive socket that is listening for connections.
    -> IO Socket
accept s = Socket.accept s >>= \(s', _) -> return s'
{- ^
    Waits until a new client shows up to connect to our server.
    Returns the socket that we use to talk to this particular peer.
-}

fin :: Socket -> IO ()
fin s = Socket.gracefulClose s finMilliseconds
{- ^
    Closes a TCP connection by sending a FIN packet, which is more
    respectful to the peer than if we were to simply ghost them.
-}

finMilliseconds :: Int
finMilliseconds = 5_000
{- ^
    Timeout for the 'fin' action.
    5 seconds = 5000 milliseconds
-}
