module Test.Suites.DecodeJsonPer
  ( suitex
  ) where

import Prelude (discard)

import Test.Suites.DecodeJsonPer.Array (suitex) as Array
import Test.Suites.DecodeJsonPer.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "decodeJsonPer" do
    Maybe.suitex
    Array.suitex
