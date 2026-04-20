{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

-- NOTE: we must NOT enable `ApplicativeDo`

module Playground1 where

import Control.Concurrent.Async (async, wait)
import Control.Functor.Linear qualified as L
import Prelude.Linear (Ur (..), move)
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Linear qualified as L

-- Dummy type for testing
type MutexKey = Int

lockScope' :: (MutexKey %1 -> L.IO (MutexKey, a)) -> L.IO a
lockScope' run = L.do
  (i, a) <- run 1
  case L.consume i of () -> L.pure ()
  L.pure a

lockScope :: (MutexKey %1 -> L.IO (MutexKey, Ur a)) -> IO a
lockScope run = L.withLinearIO (lockScope' run)

-- Consume the key and return a new one
lock ::
  MutexKey %1 ->
  L.IO MutexKey
lock x = do
  L.pure x

f2 :: IO String
f2 = do
  lockScope \key -> L.do
    key2 <- lock key
    L.fromSystemIO (putStrLn "aa")

    -- Non-linear IO with unrestricted value
    Ur line1 <- L.fromSystemIOU getLine

    -- System.IO with linear value, and the consume it in System.IO
    line2 <- L.fromSystemIO getLine
    L.fromSystemIO (putStrLnLinear line2)

    -- Fork a thread and create a new lockScope in it
    Ur _str <- L.fromSystemIOU do
      wait =<< async do
        lockScope \key -> L.do
          key2 <- lock key
          L.pure (key2, Ur "str")

    Ur () <- L.fromSystemIOU L.$ L.withLinearIO do
      L.pure L.$ move ()

    L.pure (key2, Ur line1)

putStrLnLinear :: String %1 -> IO ()
putStrLnLinear str =
  case move str of
    Ur str' -> putStrLn str'

----------------------------------------------------------------------------
-- Lock in System.IO
----------------------------------------------------------------------------

{-
NOTE: this is useless.
We can call `lock'` in System.IO and prove that the lock has been consumed exactly once
ONLY if we don't compose `lock'` with e.g. `>>=`.
See `lock'Demo` for an example
-}
lock' ::
  MutexKey %1 ->
  IO MutexKey
lock' x = do
  case move x of
    Ur x' -> pure x'

lock'Demo :: L.IO String
lock'Demo =
  lockScope' \key -> L.do
    -- This line works
    key2 <- L.fromSystemIO L.$ lock' key

    -- This doesn't work
    -- key2 <- L.fromSystemIO L.$ lock' key >>= pure

    -- This doesn't work
    -- key2 <- L.fromSystemIO L.$ do
    --   x <- lock' key
    --   pure x
    L.pure (key2, "")
