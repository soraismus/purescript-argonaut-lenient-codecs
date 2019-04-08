module Test.Suites.DecodeJsonWith'
  ( _suite
  ) where

import Prelude (pure, unit)

import Prelude (discard)

import Test.Suites.DecodeJsonWith'.Array (_suite) as Array
import Test.Suites.DecodeJsonWith'.Maybe (_suite) as Maybe
import Test.Unit (TestSuite, suite)

_suite :: TestSuite
_suite =
  suite "decodeJsonWith'" (pure unit)
    -- Maybe._suite
    -- Array._suite
