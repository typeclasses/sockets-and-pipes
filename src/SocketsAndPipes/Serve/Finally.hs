module SocketsAndPipes.Serve.Finally ( finallyInterruptible ) where

import Control.Exception.Safe ( tryAny, throw )

{- |

The second action runs even if the first action throws a synchronous exception.
But the entire sequence ends promptly when an async exception arrives.
If an async exception interrupts the first action, the second action will not run.
Both actions run in an unmasked (interruptible) context.

-}
finallyInterruptible ::
    IO a     -- ^ First action
    -> IO b  -- ^ Second action
    -> IO a
finallyInterruptible a b =
  do
    result <- tryAny a
    _ <- b
    either throw return result
