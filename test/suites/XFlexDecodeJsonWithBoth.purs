module Test.Suites.XFlexDecodeJsonWithBoth
  ( suitex
  ) where

import Prelude (discard)

import Test.Suites.XFlexDecodeJsonWithBoth.Array (suitex) as Array
import Test.Suites.XFlexDecodeJsonWithBoth.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "xFlexDecodeJsonWithBoth" do
    Maybe.suitex
    Array.suitex
