{- |
    The 'forkBracket' function ensures that if the setup
    action completes, then no matter what else happens,
    exactly one of either 'onForkFail' or 'onThreadEnd' will run.
-}
module SocketsAndPipes.Serve.ForkBracket
    ( forkBracket, Cleanup (..), ThreadId ) where

import Control.Concurrent     ( forkFinally, ThreadId )
import Control.Exception.Safe ( bracketOnError )

forkBracket ::
    IO resource           -- ^ Setup:
                          --      How to open some resource.
    -> Cleanup resource   -- ^ Cleanup:
                          --      How to close the resource.
    -> (resource -> IO x) -- ^ Action that uses the resource.
                          --      This will run in a new thread.
    -> IO ThreadId
forkBracket setup Cleanup{onForkFail, onThreadEnd} action =
    bracketOnError setup onForkFail $ \resource ->
        action resource `forkFinally` \_result ->
            onThreadEnd resource >> return ()

{- |
    A strategy for closing a resource that is opened
    by a main event loop and used in a forked thread.

    It consists of handlers for two possibilities:

      * 'onThreadEnd' runs after the thread has completed.
      * 'onForkFail' runs if the thread failed to start.
-}
data Cleanup resource
  where
    Cleanup ::
        { onForkFail :: resource -> IO x
            {- ^ Cleanup if forking fails.

                This action runs in the original thread.

                It only runs if there was an exception that
                prevented a new thread from starting.
            -}

        , onThreadEnd :: resource -> IO y
            {- ^ Cleanup after the thread terminates.

                This action runs in the forked thread.

                If the thread actually started, this cleanup
                action runs regardless of whether the thread
                terminates normally or with an exception.
            -}
        }
        -> Cleanup resource
