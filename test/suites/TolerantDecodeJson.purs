module Test.Suites.TolerantDecodeJson
  ( _suite
  ) where

import Prelude (discard)

import Test.Suites.TolerantDecodeJson.Array (_suite) as Array
import Test.Suites.TolerantDecodeJson.Maybe (_suite) as Maybe
import Test.Unit (TestSuite, suite)

_suite :: TestSuite
_suite =
  suite "tolerantDecodeJson" do
    Maybe._suite
    Array._suite
