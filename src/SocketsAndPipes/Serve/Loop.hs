module SocketsAndPipes.Serve.Loop ( run ) where

import Control.Concurrent.Async       ( race_ )
import Control.Monad                  ( forever )

import SocketsAndPipes.Serve.OnError  ( OnError, bracketOnError, forkOnError )
import SocketsAndPipes.Serve.Finally  ( finallyInterruptible )
import SocketsAndPipes.Serve.Log      ( Write, writeException )
import SocketsAndPipes.Serve.Shutdown ( withRunStateVar, waitForShutdown )
import SocketsAndPipes.Serve.Sockets  ( PeerSocket, PassiveSocket, accept,
                                        closePeerSocketPolitely,
                                        closePeerSocketAbruptly )

run ::
    Write -- ^ How to write log messages
    -> (PeerSocket -> IO a) -- ^ What to do when a new client connects.
    -> PassiveSocket -- ^ A socket that is listening for connections.
    -> IO b -- ^ Perpetually awaits new connections, forking a new thread to handle each one.
run write go s =
  withRunStateVar $ \runStateVar ->
    forever $
      bracketOnError (accept s) ({-2-} logAndCloseAbruptly write) $ \peer ->
        forkOnError ({-3-} logAndCloseAbruptly write peer) $
          race_ (waitForShutdown runStateVar *> {-4-} closePeerSocketAbruptly peer) $
            go peer `finallyInterruptible` {-1-} closePeerSocketPolitely peer

{-

Threads normally conclude with a graceful close of the peer socket. (1)

Since the graceful close procedure is a network operation that potentially
blocks for several seconds, exceptions are made in the following circumstances
where the delay would be unacceptable:

  * When a fork fails, thereby forcing the close to take place on the main thread,
    which needs to stay free to accept connections responsively. (2)

  * When the goal is to stop the forked thread as quickly as possible:

      * If an asynchronous exception has been thrown to the forked thread. (3)

      * If we're shutting down because an async exception has been thrown
        to the main thread. (4)

-}

logAndCloseAbruptly :: Write -> PeerSocket -> OnError
logAndCloseAbruptly write peer e =
  do
    closePeerSocketAbruptly peer
    writeException write e
