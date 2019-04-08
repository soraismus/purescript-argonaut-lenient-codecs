module Test.Suites.DecodeJsonPer
  ( _suite
  ) where

import Prelude (discard)

import Test.Suites.DecodeJsonPer.Array (_suite) as Array
import Test.Suites.DecodeJsonPer.Maybe (_suite) as Maybe
import Test.Unit (TestSuite, suite)

_suite :: TestSuite
_suite =
  suite "decodeJsonPer" do
    Maybe._suite
    Array._suite
