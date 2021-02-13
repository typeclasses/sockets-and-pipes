module SocketsAndPipes.Serve.Loop ( run ) where

import SocketsAndPipes.Serve.ForkBracket
    ( forkBracket, Cleanup (..), ThreadId )

import SocketsAndPipes.Serve.Sockets
    ( PeerSocket, PassiveSocket, accept,
      closePeerSocketPolitely, closePeerSocketAbruptly )

import Control.Monad ( forever )

run ::
    (PeerSocket -> IO a) -- ^ What to do when a new client connects.
    -> PassiveSocket -- ^ A socket that is listening for connections.
    -> IO b
run server s = forever (acceptAndFork s server)
{- ^
    Perpetually awaits new connections,
    forking a new thread to handle each one.
-}

acceptAndFork ::
    PassiveSocket -- ^ A socket that is listening for connections.
    -> (PeerSocket -> IO a) -- ^ What to do when a new client connects.
    -> IO ThreadId
acceptAndFork s = forkBracket (accept s) socketForkBracketCleanup
{- ^
    Waits until a new client shows up to connect to our server.
    When a peer connects, the socket for talking to them will
    be passed to the given function.
-}

socketForkBracketCleanup :: Cleanup PeerSocket
socketForkBracketCleanup = Cleanup{onForkFail, onThreadEnd}
  where
    onThreadEnd = -- At the end of the thread:
        closePeerSocketPolitely -- Politely conclude the connection.

    onForkFail = -- If an exception occurs before the thread even starts:
        closePeerSocketAbruptly -- Just close the socket abruptly.
            -- Since this happens on the main thread, we don't
            -- want to take the time to wait for a graceful close.
