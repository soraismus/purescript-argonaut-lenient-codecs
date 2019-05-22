module Test.Suites.TolerantDecodeJson
  ( suitex
  ) where

import Prelude (discard)

import Test.Suites.TolerantDecodeJson.Array (suitex) as Array
import Test.Suites.TolerantDecodeJson.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "tolerantDecodeJson" do
    Maybe.suitex
    Array.suitex
