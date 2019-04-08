module Test.Suites.XDecodeJsonWith
  ( _suite
  ) where

import Prelude (discard)

import Test.Suites.XDecodeJsonWith.Array (_suite) as Array
import Test.Suites.XDecodeJsonWith.Maybe (_suite) as Maybe
import Test.Unit (TestSuite, suite)

_suite :: TestSuite
_suite =
  suite "xDecodeJsonWith" do
    Maybe._suite
    Array._suite
