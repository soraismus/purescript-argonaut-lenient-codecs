module Test.Suites.LenientDecodeJson
  ( _suite
  ) where

import Prelude (discard)

import Test.Suites.LenientDecodeJson.Array (_suite) as Array
import Test.Suites.LenientDecodeJson.Maybe (_suite) as Maybe
import Test.Unit (TestSuite, suite)

_suite :: TestSuite
_suite =
  suite "lenientDecodeJson" do
    Maybe._suite
    Array._suite
