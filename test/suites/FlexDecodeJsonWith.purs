module Test.Suites.FlexDecodeJsonWith
  ( _suite
  ) where

import Prelude (discard)

import Test.Suites.FlexDecodeJsonWith.Array (_suite) as Array
import Test.Suites.FlexDecodeJsonWith.Maybe (_suite) as Maybe
import Test.Unit (TestSuite, suite)

_suite :: TestSuite
_suite =
  suite "flexDecodeJsonWith" do
    Maybe._suite
    Array._suite
