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

t2 :: IO Int
t2 = L.do
  x <- L.pure 1
  L.pure x

lockScope :: (Int %1 -> IO Int) -> IO Int
lockScope run = L.do
  run 1

lock ::
  Int %1 ->
  IO Int
lock x = do
  pure x

f2 :: IO Int
f2 = L.do
  lockScope \x -> L.do
    x2 <- lock x
    IO.fromSystemIO $ putStrLn "aa"
    Ur line <- IO.fromSystemIOU $ getLine
    x2 <- lock x2
    pure x2
