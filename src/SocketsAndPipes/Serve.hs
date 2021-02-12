module SocketsAndPipes.Serve
    ( {- * Serve -} serve,
      {- * Options -} ServeOptions, port
      {- * Example -} {- $example -}
    ) where

import SocketsAndPipes.Serve.Setup ( withSocketOnPort )
import SocketsAndPipes.Serve.Loop  ( run )

import Network.Socket ( Socket, PortNumber )

-- | The first argument to 'serve'.
data ServeOptions =
    ServeOnPort PortNumber

port :: PortNumber -- ^ The port number that your server will listen on
     -> ServeOptions
port = ServeOnPort

serve ::
    ServeOptions
    -> (Socket -> IO a)
            {- ^ What to do each time a new client connects to your server.
                 These actions run concurrently in separate threads. -}
    -> IO b {- ^ An action that runs forever, perpetually listening for
                 incoming connections and running the @(Socket -> IO a)@
                 function each time a new client opens a connection. -}
serve (ServeOnPort p) s = withSocketOnPort p (run s)

{- $example

Suppose you have a function that reads an HTTP request from a
socket and then writes an HTTP response back to the socket.

@handleHttpRequest :: 'Socket' -> IO ()@

Then you might start a server by running the following in GHCi:

@λ> 'serve' ('port' 8000) handleHttpRequest@

And while the server is still running, test it on the command line
like so:

> $ curl http://localhost:8000

-}
