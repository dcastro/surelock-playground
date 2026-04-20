{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

-- NOTE: we must NOT enable `ApplicativeDo`

module Playground1 where

import Control.Functor.Linear qualified as L
import Prelude.Linear (Ur (..), move)
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Linear qualified as L

-- Dummy type for testing
type MutexKey = Int

t2 :: L.IO Int
t2 = L.do
  x <- L.pure 1
  L.pure x

lockScope :: (MutexKey %1 -> L.IO (MutexKey, a)) -> L.IO a
lockScope run = L.do
  (i, a) <- run 1
  case move i of Ur _ -> L.pure ()
  L.pure a

-- Consume the key and return a new one
lock ::
  MutexKey %1 ->
  L.IO MutexKey
lock x = do
  L.pure x

f2 :: L.IO String
f2 = do
  lockScope \key -> L.do
    key2 <- lock key
    L.fromSystemIO (putStrLn "aa")
    -- This doesn't compile, `print` is not linear.
    -- IO.fromSystemIO $ print key2

    -- Non-linear IO with unrestricted value
    Ur line <- L.fromSystemIOU getLine
    -- Non-linear IO with linear value
    line <- L.fromSystemIO getLine
    -- Consume linear variable in non-linear IO
    line2 <- L.fromSystemIO getLine
    L.fromSystemIO (putStrLnLinear line2)

    Ur () <- L.fromSystemIOU L.$ L.withLinearIO do
      L.pure L.$ move ()

    L.pure (key2, line)

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
  lockScope \key -> L.do
    -- This line works
    key2 <- L.fromSystemIO L.$ lock' key

    -- This doesn't work
    -- key2 <- L.fromSystemIO L.$ lock' key >>= pure

    -- This doesn't work
    -- key2 <- L.fromSystemIO L.$ do
    --   x <- lock' key
    --   pure x
    L.pure (key2, "")
