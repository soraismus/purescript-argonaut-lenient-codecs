module Test.Suites.FlexDecodeJsonWithBoth
  ( _suite
  ) where

import Prelude (discard)

import Test.Suites.FlexDecodeJsonWithBoth.Array (_suite) as Array
import Test.Suites.FlexDecodeJsonWithBoth.Maybe (_suite) as Maybe
import Test.Unit (TestSuite, suite)

_suite :: TestSuite
_suite =
  suite "flexDecodeJsonWithBoth" do
    Maybe._suite
    Array._suite
