module Test.Suites.DecodeJsonWith
  ( suitex
  ) where

import Prelude (discard)

import Test.Suites.DecodeJsonWith.Array (suitex) as Array
import Test.Suites.DecodeJsonWith.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "decodeJsonWith" do
    Maybe.suitex
    Array.suitex
