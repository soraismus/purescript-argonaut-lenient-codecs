module Test.Suites.FlexDecodeJsonWith
  ( suitex
  ) where

import Prelude (discard)

import Test.Suites.FlexDecodeJsonWith.Array (suitex) as Array
import Test.Suites.FlexDecodeJsonWith.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "flexDecodeJsonWith" do
    Maybe.suitex
    Array.suitex
