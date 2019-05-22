module Test.Suites.DecodeJsonWithPrime
  ( suitex
  ) where

import Prelude (discard)

import Test.Suites.DecodeJsonWithPrime.Array (suitex) as Array
import Test.Suites.DecodeJsonWithPrime.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "decodeJsonWith'" do
    Maybe.suitex
    Array.suitex
