module Test.Suites.DecodeJsonWithPrime
  ( _suite
  ) where

import Prelude (discard)

import Test.Suites.DecodeJsonWithPrime.Array (_suite) as Array
import Test.Suites.DecodeJsonWithPrime.Maybe (_suite) as Maybe
import Test.Unit (TestSuite, suite)

_suite :: TestSuite
_suite =
  suite "decodeJsonWith'" do
    Maybe._suite
    Array._suite
