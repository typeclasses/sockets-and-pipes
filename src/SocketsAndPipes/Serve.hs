module SocketsAndPipes.Serve
    ( {- * Serve -} serve,
      {- * Example -} {- $example -}
      {- * Options -} ServeOptions, port
      {- * Particulars -} {- $particulars -}
      {- * Alternatives -} {- $alternatives -}
    ) where

import SocketsAndPipes.Serve.Exceptions ( displayBindFailed )
import SocketsAndPipes.Serve.Sockets    ( Socket, PortNumber, PassiveSocket,
                                          passiveSocketAddress, peerSocket )
import SocketsAndPipes.Serve.Setup      ( withSocketOnPort )
import SocketsAndPipes.Serve.Loop       ( run )
import SocketsAndPipes.Serve.Log        ( Write, withLogging )

import Control.Exception.Safe ( catch )
import Data.Functor           ( void, (<&>) )

import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified System.IO              as IO

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
    withLogging IO.stderr $ \w -> (go w `catch` ifBindFailed w)

  where
    go w =
      withSocketOnPort p $ \s ->
        do
          logBindSuccess w s
          run w (f . peerSocket) s

    ifBindFailed w =
        void . w . LT.unpack . TB.toLazyText . displayBindFailed

logBindSuccess :: Write -> PassiveSocket -> IO ()
logBindSuccess w s =
    void . w . LT.unpack . TB.toLazyText =<< displayBindSuccess s

displayBindSuccess :: PassiveSocket -> IO TB.Builder
displayBindSuccess s =
    passiveSocketAddress s <&> \a ->
        TB.fromString "The server is listening on address "
        <> TB.fromString (show a)

{- $particulars

This library ought to be mostly suitable for real applications, but keep in mind
that some of the details were chosen primarily with /Sockets and Pipes/
exercises in mind.

The server prints some messages to the standard error stream. It gives an
initial success/failure message when setting up the listening socket, and it
displays any exceptions thrown from your connection handler. We provide no means
of customizing or redirecting this output.

When the server is brought to a stop by an asynchronous exception, all of the
threads it has spawned to handle connections are immediately stopped as well.
This avoids any potentially confusing behavior that might arise if old threads
could continue running after you stop and restart a server in GHCi.

We set the `SO_REUSEADDR` flag on on the listening socket. This has some
security-related ramifications, but it lets you restart a server quickly. By
default, the OS would require some amount of time to elapse before a
previously-used port is allowed to be bound again.

When possible, the listening socket is bound to an IPv6 address with IPv4 also
enabled. This allows us to run an HTTP server that can be reached at both the
IPv4 address @http:/\/127.0.0.1:8000@ and the IPv6 adddress @http:/\/[::1]:8000@.

-}

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

Some other packages offer different choices and configuration options:

  * <https://hackage.haskell.org/package/network-simple/docs/Network-Simple-TCP.html network-simple>
  * <https://hackage.haskell.org/package/network-run/docs/Network-Run-TCP.html network-run>
  * <https://hackage.haskell.org/package/streaming-commons/docs/Data-Streaming-Network.html streaming-commons>

-}
