{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Surelock where

import Control.Concurrent (MVar)
import Control.Concurrent qualified as MVar
import Control.Functor.Linear qualified as L
import Data.Kind (Type)
import GHC.TypeLits
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Linear qualified as L

-- Notes:
--  * Do not export the constructor
--  * Do not implement `Consumable` / `Movable`
data MutexKey (lvl :: Nat) (scope :: Type) = MutexKey

data Mutex (lvl :: Nat) a = Mutex {getVar :: MVar a}

-- | Consume the key and return a new key (with an increased level) in linear IO
lock ::
  forall a keyLvl mutexLvl scope.
  (keyLvl <= mutexLvl) =>
  MutexKey keyLvl scope %1 ->
  Mutex mutexLvl a ->
  L.IO (Ur a, MutexKey (mutexLvl + 1) scope)
lock MutexKey m = L.do
  a <- L.fromSystemIOU L.$ MVar.takeMVar m.getVar
  L.pure (a, MutexKey)

withLock ::
  forall a keyLvl mutexLvl lastLvl scope ret.
  (keyLvl <= mutexLvl) =>
  MutexKey keyLvl scope %1 ->
  Mutex mutexLvl a ->
  (Ur a %1 -> MutexKey (mutexLvl + 1) scope %1 -> L.IO (Ur a, Ur ret, MutexKey lastLvl scope)) ->
  L.IO (Ur ret, MutexKey lastLvl scope)
withLock MutexKey m run = L.do
  a <- L.fromSystemIOU L.$ MVar.takeMVar m.getVar
  (Ur a', ret, key) <- run a MutexKey

  L.fromSystemIO L.$ MVar.putMVar m.getVar a'

  L.pure (ret, key)

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
    MutexKey 0 scope ->
    L.IO (a, MutexKey lvl scope)
  ) ->
  L.IO a
lockScope' run = L.do
  let key = MutexKey @0
  (a, MutexKey) <- run key
  L.pure a

-- | Like `lockScope'`, but returns in non-linear `System.IO`.
lockScope ::
  forall a lvl.
  ( forall (scope :: Type).
    MutexKey 0 scope ->
    L.IO (Ur a, MutexKey lvl scope)
  ) ->
  IO a
lockScope run =
  L.withLinearIO (lockScope' run)

----------------------------------------------------------------------------
-- Usage examples
----------------------------------------------------------------------------

usage1 :: IO ()
usage1 = do
  mutex <- mkMutex @0 "hello"
  lockScope \key -> L.do
    (Ur str, key) <- lock key mutex
    L.fromSystemIO (putStrLn str)
    L.pure (Ur (), key)

-- This doesn't compile, we can't acquire locks out of order
-- usage2 :: IO ()
-- usage2 = do
--   m1 <- mkMutex @0 "hello"
--   m2 <- mkMutex @1 "hello"
--   lockScope \key -> L.do
--     (Ur _, key) <- lock key m2
--     (Ur _, key) <- lock key m1
--     L.pure (Ur (), key)

usage3 :: IO ()
usage3 = do
  m1 <- mkMutex @0 "hello"
  m2 <- mkMutex @1 "world"
  lockScope \key -> L.do
    withLock key m1 \(Ur str1) key -> L.do
      (ret, key) <- withLock key m2 \(Ur str2) key -> L.do
        L.fromSystemIO (putStrLn $ str1)
        L.pure (Ur str2, Ur (), key)
      L.pure (Ur str1, ret, key)
