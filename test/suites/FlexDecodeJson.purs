module Test.Suites.FlexDecodeJson
  ( _suite
  ) where

import Prelude (discard)

import Test.Suites.FlexDecodeJson.Array (_suite) as Array
import Test.Suites.FlexDecodeJson.Maybe (_suite) as Maybe
import Test.Unit (TestSuite, suite)

_suite :: TestSuite
_suite =
  suite "flexDecodeJson" do
    Maybe._suite
    Array._suite
