{- |

    The 'PassiveSocket' and 'PeerSocket' newtypes
    distinguish passive sockets from peer sockets.

    This module also includes a few basic operations
    involving these types.

-}
module SocketsAndPipes.Serve.Sockets
    (

      Socket,
      PortNumber,

      PassiveSocket (..), closePassiveSocket,
                          passiveSocketAddress,

      PeerSocket    (..), closePeerSocketAbruptly,
                          closePeerSocketPolitely,

      accept

    ) where

import Network.Socket ( Socket, PortNumber )
import qualified Network.Socket as Socket

newtype PassiveSocket = PassiveSocket { passiveSocket :: Socket }
    -- ^ A passive socket that is listening for connections.

newtype PeerSocket = PeerSocket { peerSocket :: Socket }
    -- ^ A socket that we're using to talk to a client
    --   that has connected to our server.

closePassiveSocket :: PassiveSocket -> IO ()
closePassiveSocket = Socket.close . passiveSocket

closePeerSocketAbruptly :: PeerSocket -> IO ()
closePeerSocketAbruptly = Socket.close . peerSocket

closePeerSocketPolitely :: PeerSocket -> IO ()
closePeerSocketPolitely s =
    Socket.gracefulClose (peerSocket s) finMilliseconds
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

accept :: PassiveSocket -> IO PeerSocket
accept s =
    Socket.accept (passiveSocket s) >>=
    \(s', _) -> return (PeerSocket s')
{- ^
    Waits until a new client shows up to connect to our server.
    Returns the socket that we use to talk to this particular peer.
-}

passiveSocketAddress :: PassiveSocket -> IO Socket.SockAddr
passiveSocketAddress = Socket.getSocketName . passiveSocket
