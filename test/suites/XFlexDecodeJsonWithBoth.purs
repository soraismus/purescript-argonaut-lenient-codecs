module Test.Suites.XFlexDecodeJsonWithBoth
  ( _suite
  ) where

import Prelude (discard)

import Test.Suites.XFlexDecodeJsonWithBoth.Array (_suite) as Array
import Test.Suites.XFlexDecodeJsonWithBoth.Maybe (_suite) as Maybe
import Test.Unit (TestSuite, suite)

_suite :: TestSuite
_suite =
  suite "xFlexDecodeJsonWithBoth" do
    Maybe._suite
    Array._suite
