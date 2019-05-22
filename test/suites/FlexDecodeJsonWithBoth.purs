module Test.Suites.FlexDecodeJsonWithBoth
  ( suitex
  ) where

import Prelude (discard)

import Test.Suites.FlexDecodeJsonWithBoth.Array (suitex) as Array
import Test.Suites.FlexDecodeJsonWithBoth.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "flexDecodeJsonWithBoth" do
    Maybe.suitex
    Array.suitex
