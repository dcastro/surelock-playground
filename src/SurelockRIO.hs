{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Surelock.RIO where

import Control.Concurrent (MVar)
import Control.Concurrent qualified as MVar
import Control.Functor.Linear qualified as L
import Data.Kind (Type)
import GHC.TypeLits
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Linear qualified as L
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear qualified as RIO
import System.IO.Resource.Linear.Internal qualified as Internal

-- Notes:
--  * Do not export the constructor
--  * Do not implement `Consumable` / `Movable`
data MutexKey (lvl :: Nat) (scope :: Type) = MutexKey

newtype Mutex (lvl :: Nat) a = Mutex {getVar :: MVar a}

data MG a = MG
  { committedValue :: Ur a,
    var :: MVar a
  }

-- type MutexGuard a = RIO.Resource (MG a)
data MutexGuard a = MutexGuard
  { resource :: RIO.Resource (MG a),
    newValue :: Ur a
  }

-- | Consume the key and return a new key (with an increased level) in linear IO
lock ::
  forall a keyLvl mutexLvl scope.
  (keyLvl <= mutexLvl) =>
  MutexKey keyLvl scope %1 ->
  Mutex mutexLvl a ->
  RIO (MutexGuard a, MutexKey (mutexLvl + 1) scope)
lock MutexKey m = L.do
  resource <- RIO.unsafeAcquire acq rel
  -- NOTE: unsafeAcquire does not let us return additional data (e.g. `committedValue`), so
  -- we have to retrieve the `committedValue` from the resource after acquiring it.
  (Ur committedValue, resource) <- RIO.unsafeFromSystemIOResource (\mg -> pure (L.unur mg.committedValue)) resource

  L.pure (MutexGuard {resource, newValue = Ur committedValue}, MutexKey)
  where
    acq :: L.IO (Ur (MG a))
    acq = L.do
      Ur a <- L.fromSystemIOU L.$ MVar.takeMVar m.getVar
      L.pure (Ur (MG {committedValue = Ur a, var = m.getVar}))

    rel :: MG a -> L.IO ()
    rel (MG (Ur committedValue) var) =
      L.void L.$ L.fromSystemIO L.$ MVar.putMVar var committedValue

readGuard :: MutexGuard a %1 -> RIO (Ur a, MutexGuard a)
readGuard (MutexGuard resource (Ur newValue)) =
  L.pure (Ur newValue, MutexGuard {resource, newValue = Ur newValue})

writeGuard :: MutexGuard a %1 -> a -> RIO (MutexGuard a)
writeGuard (MutexGuard resource (Ur _)) newValue =
  L.pure (MutexGuard {resource, newValue = Ur newValue})

releaseGuard :: MutexGuard a %1 -> RIO ()
releaseGuard (MutexGuard (Internal.UnsafeResource key res) (Ur newValue)) =
  RIO.release (Internal.UnsafeResource key (res {committedValue = Ur newValue}))

mkMutex :: forall lvl a. a -> IO (Mutex lvl a)
mkMutex a = do
  var <- MVar.newMVar a
  pure (Mutex var)

-- | Creates a new lock scope with a key of level 0, and runs the given function with it.
--  The key can be used to lock mutexes with `lock`.
-- The final key must be returned.
--
-- The key is indexed by a rank-2 type variable `scope` to prevent it from
-- being used outside of the scope of `lockScope`.
lockScope' ::
  forall a lvl.
  ( forall (scope :: Type).
    MutexKey 0 scope %1 ->
    RIO (a, MutexKey lvl scope)
  ) ->
  RIO a
lockScope' run = L.do
  let key = MutexKey @0
  (a, MutexKey) <- run key
  L.pure a

-- | Like `lockScope'`, but returns in non-linear `System.IO`.
lockScope ::
  forall a lvl.
  ( forall (scope :: Type).
    MutexKey 0 scope %1 ->
    RIO (Ur a, MutexKey lvl scope)
  ) ->
  IO a
lockScope run =
  RIO.run (lockScope' run)

----------------------------------------------------------------------------
-- Usage examples
----------------------------------------------------------------------------

-- Acquire 1 lock
usage1 :: IO ()
usage1 = do
  mutex <- mkMutex @0 "hello"
  lockScope \key -> L.do
    (mg, key) <- lock key mutex
    (Ur str, mg) <- readGuard mg
    Internal.unsafeFromSystemIO (putStrLn str)
    mg <- writeGuard mg "world"
    releaseGuard mg
    L.pure (Ur (), key)

-- This doesn't compile, we can't acquire locks out of order
-- usage2 :: IO ()
-- usage2 = do
--   m1 <- mkMutex @0 "hello"
--   m2 <- mkMutex @1 "world"
--   lockScope \key -> L.do
--     (mg2, key) <- lock key m2
--     (mg1, key) <- lock key m1
--     releaseGuard mg1
--     releaseGuard mg2
--     L.pure (Ur (), key)

-- Acquire 2 locks
usage3 :: IO ()
usage3 = do
  m1 <- mkMutex @0 "hello"
  m2 <- mkMutex @1 "world"
  lockScope \key -> L.do
    (mg1, key) <- lock key m1
    (mg2, key) <- lock key m2
    (Ur str1, mg1) <- readGuard mg1
    (Ur str2, mg2) <- readGuard mg2

    Internal.unsafeFromSystemIO (putStrLn $ str1 <> " " <> str2)

    releaseGuard mg1
    releaseGuard mg2

    L.pure (Ur (), key)
