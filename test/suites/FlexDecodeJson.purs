module Test.Suites.FlexDecodeJson
  ( suitex
  ) where

import Prelude (discard)

import Test.Suites.FlexDecodeJson.Array (suitex) as Array
import Test.Suites.FlexDecodeJson.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "flexDecodeJson" do
    Maybe.suitex
    Array.suitex
