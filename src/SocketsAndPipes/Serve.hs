module SocketsAndPipes.Serve
    ( {- * Serve -} serve,
      {- * Example -} {- $example -}
      {- * Options -} ServeOptions, port
      {- * Alternatives -} {- $alternatives -}
    ) where

import SocketsAndPipes.Serve.Exceptions ( displayBindFailed )
import SocketsAndPipes.Serve.Sockets    ( Socket, PortNumber, PassiveSocket,
                                          passiveSocketAddress, peerSocket )
import SocketsAndPipes.Serve.Setup      ( withSocketOnPort )
import SocketsAndPipes.Serve.Loop       ( run )

import Control.Exception.Safe ( catch )

import qualified Data.Text.Lazy.IO      as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified System.IO              as IO

import Prelude hiding (print)

-- | The first argument to 'serve'.
data ServeOptions =
    ServeOnPort PortNumber
{- This type is abstract so that we might add more options in
   the future without disturbing the users of this library. -}

port :: PortNumber -- ^ The port number that your server will listen on
     -> ServeOptions
port = ServeOnPort

serve ::
    ServeOptions
    -> (Socket -> IO ())
            {- ^ What to do each time a new client connects to your server.
                 These actions run concurrently in separate threads. -}
    -> IO ()
            {- ^ Perpetually listens for incoming connections and runs
                 the @(Socket -> IO ())@ function each time a new client
                 opens a connection. -}
serve (ServeOnPort p) f =
    go `catch` ifBindFailed

  where
    go = withSocketOnPort p $ \s ->
      do
        print =<< displayBindSuccess s
        run (f . peerSocket) s

    ifBindFailed = print . displayBindFailed

print :: TB.Builder -> IO ()
print = LT.hPutStrLn IO.stderr . TB.toLazyText

displayBindSuccess :: PassiveSocket -> IO TB.Builder
displayBindSuccess s =
    passiveSocketAddress s >>= \a ->
        return $
            TB.fromString "The server is listening on address "
            <> TB.fromString (show a)

{- $example

Suppose you have a function that reads an HTTP request from a
socket and then writes an HTTP response back to the socket.

@handleHttpRequest :: 'Socket' -> IO ()@

Then you might start a server by running the following in GHCi:

@
λ> import "SocketsAndPipes.Serve"
λ> 'serve' ('port' 8000) handleHttpRequest
@

And while the server is still running, test it on the command line
like so:

> $ curl http://localhost:8000

-}

{- $alternatives

The 'serve' function here is somewhat narrowly tailored
to fit our purposes in /Sockets and Pipes/.
Some other packages have more expansive offerings:

  * <https://hackage.haskell.org/package/network-simple network-simple>
  * <https://hackage.haskell.org/package/network-run network-run>

-}
