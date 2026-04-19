{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- NOTE: we must NOT enable `ApplicativeDo`

module Playground1 where

import Control.Functor.Linear qualified as L
import Prelude.Linear hiding (IO)
import System.IO.Linear (IO)

t2 :: IO Int
t2 = L.do
  x <- L.pure 1
  L.pure x
