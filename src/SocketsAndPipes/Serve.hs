module SocketsAndPipes.Serve ( serve ) where

import SocketsAndPipes.Serve.Setup ( withSocketOnPort )
import SocketsAndPipes.Serve.Loop  ( run )

import Network.Socket ( Socket, PortNumber )

serve ::
    PortNumber
        {- ^
            The port number that your server will listen on.
        -}
    -> (Socket -> IO a)
        {- ^
            What to do each time a new client connects to your server.
            These actions run concurrently in separate threads.
        -}
    -> IO b
        {- ^
            An action that runs forever, perpetually listening for
            incoming connections and running the @(Socket -> IO a)@
            function each time a new client opens a connection.
        -}

serve port server = withSocketOnPort port (run server)

{- ^
    For example, suppose you have a function that reads an HTTP request
    from a socket and then writes an HTTP response back to the socket.

    @handleHttpRequest :: 'Socket' -> IO ()@

    Then you might start a server by running the following in GHCi:

    @λ> 'serve' 8000 handleHttpRequest@

    And while the server is still running, test it on the command line
    like so:

    > $ curl http://localhost:8000
-}
