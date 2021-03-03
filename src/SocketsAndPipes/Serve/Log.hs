{- |

If multiple threads are printing to the same handle at once, the messages can
interleave and become nonsensical. So instead of printing directly, we write
messages to a queue, and a single thread is responsible for reading from the
queue and writing to a handle.

-}
module SocketsAndPipes.Serve.Log ( withLogging, Write, writeException ) where

import Data.Functor                  ( ($>), (<&>), void )
import Control.Applicative           ( (<|>) )
import Control.Concurrent.Async      ( withAsync, wait )
import Control.Concurrent.STM.TQueue ( TQueue, newTQueue, writeTQueue, readTQueue, isEmptyTQueue )
import Control.Concurrent.STM.TVar   ( TVar, newTVar, writeTVar, readTVar )
import Control.Monad                 ( join, guard )
import Control.Monad.STM             ( STM, atomically )
import Control.Exception.Safe        ( SomeException, finally, displayException )
import System.IO                     ( Handle, hPutStrLn )

-- | The internal state of the logging system.
data Log =
   Log
     { messageQueue :: TQueue Message
     , runStateVar :: TVar RunState
     }

data RunState =
    Run      {- ^ Log state is initially 'Run'. -}
  | Shutdown {- ^ The state changes to 'Shutdown' when the rest of the program
                  has ended. In the 'Shutdown' state, the log-printing thread
                  continues printing until the queue is empty, then stops. -}
  deriving Eq

-- | A message that can be written to a log.
type Message = String

-- | Value returned after a message is written to the log.
data MessageWritten = MessageWritten

-- | Function that writes a message to the log.
type Write = Message -> IO MessageWritten

-- | Value returned at the very end when logging is shut down and the queue is empty.
data LoggingFinished = LoggingFinished

-- | A log is initially created with an empty queue, in the 'Run' state.
newLog :: STM Log
newLog = Log <$> newTQueue <*> newTVar Run

-- | This function typically encloses an entire `main` action.
withLogging ::
    Handle -- ^ What handle the log output shall be written to
    -> (Write -> IO ()) -- ^ Continuation provided with a thread-safe print function
    -> IO ()
withLogging h go =
  do
    l <- atomically newLog
    withAsync (go (writeToLog l)) $ \a1 ->
      withAsync (printFromLog h l) $ \a2 ->
        do
          wait a1 `finally` requestLogStop l
          LoggingFinished <- wait a2
          return ()

requestLogStop :: Log -> IO ()
requestLogStop l = atomically (requestLogStopSTM l)

requestLogStopSTM :: Log -> STM ()
requestLogStopSTM Log{ runStateVar } = writeTVar runStateVar Shutdown

writeToLog :: Log -> Write
writeToLog l x = atomically (writeToLogSTM l x)

writeToLogSTM :: Log -> Message -> STM MessageWritten
writeToLogSTM Log{ messageQueue } message =
    writeTQueue messageQueue message $> MessageWritten

printFromLog :: Handle -> Log -> IO LoggingFinished
printFromLog h l@Log{ messageQueue } = continue
  where
    continue :: IO LoggingFinished
    continue = join $ atomically (a <|> b)

    a = readTQueue messageQueue <&> \x ->
          hPutStrLn h x *> continue
    b = (guard =<< isReadyToStop l) $>
          return LoggingFinished

shutdownRequested :: Log -> STM Bool
shutdownRequested Log{ runStateVar } =
    readTVar runStateVar <&> \s ->
        s == Shutdown

isReadyToStop :: Log -> STM Bool
isReadyToStop l@Log{ messageQueue } =
    (&&) <$> shutdownRequested l
         <*> isEmptyTQueue messageQueue

writeException :: Write -> SomeException -> IO ()
writeException w e = void (w (displayException e))
