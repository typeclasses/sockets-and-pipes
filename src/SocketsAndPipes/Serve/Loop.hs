module SocketsAndPipes.Serve.Loop ( run ) where

import Control.Monad  ( forever )
import Network.Socket ( Socket )

import qualified Control.Concurrent     as Concurrent
import qualified Control.Exception.Safe as Exception
import qualified Network.Socket         as Socket

run :: (Socket -> IO a) -> Socket -> IO b
run server s = forever $ withNewClient s (forkFin server)

withNewClient :: Socket -> (Socket -> IO a) -> IO a
withNewClient s = Exception.bracketOnError (accept s) Socket.close

accept :: Socket -> IO Socket
accept s = Socket.accept s >>= \(s', _) -> return s'

forkFin :: (Socket -> IO a) -> Socket -> IO ()
forkFin f s = Concurrent.forkFinally (f s) (\_ -> fin s) *> return ()
{- ^
    Runs the function `f` in a new thread, and
    closes the socket politely at the end.
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
