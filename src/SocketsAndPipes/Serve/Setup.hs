module SocketsAndPipes.Serve.Setup ( withSocketOnPort ) where

import SocketsAndPipes.Serve.Sockets
  ( PortNumber, Socket, PassiveSocket (..), closePassiveSocket )

import Control.Monad  ( (>=>), when )
import Data.Foldable  ( asum )
import Data.Function  ( on )

import qualified Control.Exception.Safe as Exception
import qualified Data.List              as List
import qualified Network.Socket         as Socket

withSocketOnPort :: PortNumber -> (PassiveSocket -> IO a) -> IO a
withSocketOnPort port = Exception.bracket (bindToPort port) closePassiveSocket

bindToPort :: PortNumber -> IO PassiveSocket
bindToPort = addrsForPort >=> chooseAddrAndBind

addrsForPort :: PortNumber -> IO [Socket.AddrInfo]
addrsForPort port = Socket.getAddrInfo hints hostName serviceName
  where
    hints       = Just serverAddrHints   :: Maybe Socket.AddrInfo
    hostName    = Nothing                :: Maybe Socket.HostName
    serviceName = Just (show port)       :: Maybe Socket.ServiceName
{- ^
    The first thing we have to do when starting a server is figure
    out exactly what network address to listen on.

    We've been given a port number, but that's only half the story;
    a network address actually include a lot more obnoxious details
    in addition to the port number.

    'addrsForPort' uses the 'S.getAddrInfo' function from the network
    library to find a list of possible addresses for us to choose from.
-}

serverAddrHints :: Socket.AddrInfo
serverAddrHints =
    Socket.defaultHints{ Socket.addrSocketType, Socket.addrFlags }
  where
    addrSocketType = Socket.Stream :: Socket.SocketType
        {- A "stream" socket uses TCP to make sure all the
           packets arrive in the right order. -}
    addrFlags = [Socket.AI_PASSIVE] :: [Socket.AddrInfoFlag]
        {- A "passive" socket is a socket that will be
           used to listen for incoming connections. -}

chooseAddrAndBind :: [Socket.AddrInfo] -> IO PassiveSocket
chooseAddrAndBind =
    asum . map bindToAddr . List.sortBy (compare `on` addrPreference)

addrPreference :: Socket.AddrInfo -> Int
addrPreference addr =
    case Socket.addrFamily addr of
        Socket.AF_INET6 -> 1 {- IPv6 is best, because these addresses can
                                accept both IPv4 and IPv6 connections. -}
        Socket.AF_INET  -> 2 {- IPv4 is next best, if IPv6 is not supported. -}
        _               -> 3 {- Other addressing schemes are unfamiliar. -}
{- ^
    Assigns a ranking to each address, indicating our relative preference.
    A lesser number indicates a more preferable address.
-}

bindToAddr :: Socket.AddrInfo -> IO PassiveSocket
bindToAddr addr =
    Exception.bracketOnError (Socket.openSocket addr) Socket.close $ \s ->
        initServerSocket addr s *> return (PassiveSocket s)

initServerSocket :: Socket.AddrInfo -> Socket -> IO ()
initServerSocket addr s =
  do
    setReuseAddr s       -- Disable some safety to permit fast restarts.
    setKeepAlive s       -- Send empty packets to keep connections alive.
    setNoDelay s         -- Send bytes immediately without buffering.
    allowIPv4and6 addr s -- If it's an IPv6 address, enable IPv4 also.
    bind addr s          -- Assign the address to the socket.
    listen s             -- Announce willingness to receive connections.

bind :: Socket.AddrInfo -> Socket -> IO ()
bind addr s = Socket.bind s (Socket.addrAddress addr)

allowIPv4and6 :: Socket.AddrInfo -> Socket -> IO ()
allowIPv4and6 addr s =
    when (Socket.addrFamily addr == Socket.AF_INET6) $
        Socket.setSocketOption s Socket.IPv6Only 0

setReuseAddr :: Socket -> IO ()
setReuseAddr s = Socket.setSocketOption s Socket.ReuseAddr 1
{- ^
    By default, the operating system will not let us restart our server and
    bind to the same address immediately, because the new process will
    receive any TCP packets that were in flight during the restart, which
    is typically undesirable.

    Overriding the default behavior like this is not really safe!
    But it lets us restart our server quickly :)
-}

setKeepAlive :: Socket -> IO ()
setKeepAlive s = Socket.setSocketOption s Socket.KeepAlive 1
{- ^
    This enables a nice TCP feature: if there is a long period of time
    with no activity on the socket, the OS will occasionally send an
    empty packet. This has two benefits:

      1. It lets the peer know that we're still here; otherwise the peer will
         close the connection, assuming that we've abandoned the conversation.

      2. If lets us know whether the peer is still there. If we don't receive
         an acknowledgement of the empty packet, we can close the connection.
-}

setNoDelay :: Socket -> IO ()
setNoDelay s = Socket.setSocketOption s Socket.NoDelay 1
{- ^

    Since it's more efficient to transmit a few large packets than many
    small packets, the OS doesn't always send your bytes right away when
    you write to a socket; By default, it make some effort to group
    together small writes into larger packets.

    The downside of this optimization is that it means sometimes we don't
    immediately see the effect of writing to a socket. For experimental
    and demonstration purposes, this can be quite undesirable, so we use
    this setting to disable the feature.
-}

listen :: Socket -> IO ()
listen s = Socket.listen s listenBacklog
{- ^
    Informs the operating system that this socket will be used to
    accept incoming connection requests.

    Such as socket is called a "passive socket".
-}

listenBacklog :: Int
listenBacklog = 1_024
{- ^
    The 'S.accept' function pulls sockets from a queue maintained by
    the operating system. This is the size we are requesting for that queue.
    (The OS might not actually give us a queue as big as we ask for.)
-}
