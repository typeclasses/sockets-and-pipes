module SocketsAndPipes.Serve.OnError ( OnError, bracketOnError, forkOnError ) where

import Control.Exception.Safe ( SomeException )

import Data.Foldable ( for_ )
import Data.Functor  ( void )

import qualified Control.Concurrent     as Concurrent
import qualified Control.Exception.Safe as Exception

-- | Some effect to perform only if an exception is thrown.
type OnError = SomeException -> IO ()

bracketOnError ::
    IO resource -- ^ Create some resource
    -> (resource -> OnError) -- ^ Effect to run if the action fails
    -> (resource -> IO c) -- ^ The action
    -> IO c
bracketOnError setup onError =
    Exception.bracketWithError setup $ \exceptionMaybe resource ->
        for_ exceptionMaybe $ onError resource

forkOnError ::
    OnError -- ^ Effect to run (in the forked thread) if the action fails
    -> IO a -- ^ The action (to run in a forked thread)
    -> IO ()
forkOnError onError action = void $
    Concurrent.forkFinally action $ \exceptionEither ->
        for_ (either Just (\_ -> Nothing) exceptionEither) onError
