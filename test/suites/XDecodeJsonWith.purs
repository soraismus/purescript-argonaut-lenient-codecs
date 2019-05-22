module Test.Suites.XDecodeJsonWith
  ( suitex
  ) where

import Prelude (discard)

import Test.Suites.XDecodeJsonWith.Array (suitex) as Array
import Test.Suites.XDecodeJsonWith.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "xDecodeJsonWith" do
    Maybe.suitex
    Array.suitex
