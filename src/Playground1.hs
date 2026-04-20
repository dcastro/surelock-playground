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

t2 :: L.IO Int
t2 = L.do
  x <- L.pure 1
  L.pure x

lockScope :: (Int %1 -> L.IO (Int, a)) -> L.IO a
lockScope run = L.do
  (i, a) <- run 1
  case move i of Ur _ -> L.pure ()
  L.pure a

lock ::
  Int %1 ->
  L.IO Int
lock x = do
  L.pure x

lock' ::
  Int %1 ->
  IO Int
lock' x = do
  case move x of
    Ur x' -> pure x'

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

    -- Use key in non-linear IO
    key3 <- L.fromSystemIO L.$ lock' key2

    key4 <- lock key3
    L.pure (key4, line)

putStrLnLinear :: String %1 -> IO ()
putStrLnLinear str =
  case move str of
    Ur str' -> putStrLn str'
