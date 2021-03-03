module SocketsAndPipes.Serve.Shutdown ( RunStateVar, withRunStateVar, waitForShutdown ) where

import Control.Monad               ( guard )
import Control.Monad.STM           ( atomically )
import Control.Concurrent.STM.TVar ( TVar, newTVar, writeTVar, readTVar )

import qualified Control.Exception.Safe as Exception

data RunState =
    Run      {- ^ The state is initially 'Run'. -}
  | Shutdown {- ^ The state changes to 'Shutdown' when the main event loop
                  is killed by an async exception. Entering the 'Shutdown'
                  state causes all of the child threads to stop. -}
  deriving Eq

type RunStateVar = TVar RunState

withRunStateVar :: (RunStateVar -> IO a) -> IO a
withRunStateVar f =
  do
    runStateVar <- atomically (newTVar Run)
    f runStateVar `Exception.finally` atomically (writeTVar runStateVar Shutdown)

waitForShutdown :: RunStateVar -> IO ()
waitForShutdown runStateVar =
    atomically $
      do
        x <- readTVar runStateVar
        guard (x == Shutdown)
