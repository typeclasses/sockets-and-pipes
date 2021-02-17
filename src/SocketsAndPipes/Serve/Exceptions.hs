module SocketsAndPipes.Serve.Exceptions
  (
    -- * Types
    BindFailed (..), AddrTried (..),

    -- * Functions related to the types
    displayBindFailed, displayAddrTried,

    -- * General functions for working with exceptions
    overException, firstSuccessOrAllExceptions

  ) where

import Control.Exception.Safe
    ( Exception (displayException), SomeException, catch, throw )

import Data.Foldable ( fold )

import qualified Data.Sequence          as Seq
import qualified Data.Foldable          as Seq ( toList )
import qualified Data.List              as List
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Network.Socket         as Socket

data BindFailed =
    BindFailed
        { bindAddrsTried :: [AddrTried]
        }
    deriving Show

instance Exception BindFailed
  where
    displayException = LT.unpack . TB.toLazyText . displayBindFailed

data AddrTried =
    AddrTried
        { addrTried :: Socket.AddrInfo
        , addrException :: SomeException
        }
    deriving Show

instance Exception AddrTried
  where
    displayException = LT.unpack . TB.toLazyText . displayAddrTried

displayBindFailed :: BindFailed -> TB.Builder
displayBindFailed BindFailed{ bindAddrsTried }
    | null bindAddrsTried = displayBindFailedNoAddresses
    | otherwise           = displayBindFailedWithAddrs bindAddrsTried

displayBindFailedNoAddresses :: TB.Builder
displayBindFailedNoAddresses =
  TB.fromString
    "Failed to set up a passive socket for the server \
    \because no candidate addresses were found."

displayBindFailedWithAddrs :: [AddrTried] -> TB.Builder
displayBindFailedWithAddrs bindAddrsTried =
    TB.fromString "Failed to set up a passive socket for the server. \
                  \The following addresses were tried:\n" <>
    fold
      (
        List.intersperse
            (TB.fromString "\n")
            (
              List.map
                  ( \AddrTried{ addrTried, addrException } ->
                        TB.fromString " ❌ " <>
                        TB.fromString (show (Socket.addrAddress addrTried)) <>
                        TB.fromString " — " <>
                        TB.fromString (displayException addrException)
                  )
                  bindAddrsTried
            )
      )

displayAddrTried :: AddrTried -> TB.Builder
displayAddrTried AddrTried{ addrTried, addrException } =
    TB.fromString (show addrTried) <> TB.fromString ": "
    <> TB.fromString (displayException addrException)

overException :: (Exception e1, Exception e2) =>
    (e1 -> e2) -- ^ How to turn the exception
               --   into a different exception
    -> IO a -- ^ Action that might throw the first exception
    -> IO a -- ^ Action that might throw the second exception
overException f a = catch a (throw . f)
{- ^
    If the action throws an exception, turn it into a different
    exception. This is useful to add information that explains
    the context in which the original exception occurred.
-}

firstSuccessOrAllExceptions :: (Exception e1, Exception e2) =>
    ([e1] -> e2) -- ^ How to collect the many exceptions into one
    -> [IO a] -- ^ Many actions that may fail
    -> IO a -- ^ The result from the first action that succeeds,
            --   or else throws a collection of all the exceptions.
firstSuccessOrAllExceptions f = go Seq.empty
  where
    go exs [] = throw (f (Seq.toList exs))
    go exs (a : as) = catch a (\ex -> go (exs Seq.|> ex) as)
