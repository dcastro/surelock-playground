{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- NOTE: we must NOT enable `ApplicativeDo`

module Playground1 where

import Control.Functor.Linear as L
import Prelude.Linear hiding (IO)
import System.IO.Linear (IO)
import System.IO.Linear qualified as IO
import Prelude qualified as P

t2 :: IO Int
t2 = L.do
  x <- L.pure 1
  L.pure x

lockScope :: (Int %1 -> IO (Int, a)) -> IO a
lockScope run = L.do
  (i, a) <- run 1
  case move i of Ur _ -> pure ()
  pure a

lock ::
  Int %1 ->
  IO Int
lock x = do
  pure x

lock' ::
  Int %1 ->
  P.IO Int
lock' x = do
  case move x of
    Ur x' -> P.pure x'

f2 :: IO String
f2 = L.do
  lockScope \key -> L.do
    key2 <- lock key
    IO.fromSystemIO $ putStrLn "aa"
    -- This doesn't compile, `print` is not linear.
    -- IO.fromSystemIO $ print key2

    -- Non-linear IO with unrestricted value
    Ur line <- IO.fromSystemIOU getLine
    -- Non-linear IO with linear value
    line <- IO.fromSystemIO getLine
    -- Consume linear variable in non-linear IO
    line2 <- IO.fromSystemIO getLine
    IO.fromSystemIO $ putStrLnLinear line2

    Ur () <- IO.fromSystemIOU $ IO.withLinearIO do
      pure $ move ()

    -- Use key in non-linear IO
    key3 <- IO.fromSystemIO $ lock' key2

    key4 <- lock key3
    pure (key4, line)

putStrLnLinear :: String %1 -> P.IO ()
putStrLnLinear str =
  case move str of
    Ur str' -> putStrLn str'
