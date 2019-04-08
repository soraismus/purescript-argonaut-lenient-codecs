module Test.Main
  ( main
  ) where

import Prelude (Unit, discard)

import Effect (Effect)
import Test.Suites.DecodeJsonPer (_suite) as DecodeJsonPer
import Test.Suites.DecodeJsonWith (_suite) as DecodeJsonWith
import Test.Suites.DecodeJsonWithPrime (_suite) as DecodeJsonWith'
import Test.Suites.FlexDecodeJson (_suite) as FlexDecodeJson
import Test.Suites.FlexDecodeJsonWith (_suite) as FlexDecodeJsonWith
import Test.Suites.FlexDecodeJsonWithBoth (_suite) as FlexDecodeJsonWithBoth
import Test.Suites.LenientDecodeJson (_suite) as LenientDecodeJson
import Test.Suites.XDecodeJsonWith (_suite) as XDecodeJsonWith
import Test.Suites.XFlexDecodeJsonWithBoth (_suite) as XFlexDecodeJsonWithBoth
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  DecodeJsonPer._suite
  DecodeJsonWith._suite
  DecodeJsonWith'._suite
  FlexDecodeJson._suite
  FlexDecodeJsonWith._suite
  FlexDecodeJsonWithBoth._suite
  LenientDecodeJson._suite
  XDecodeJsonWith._suite
  XFlexDecodeJsonWithBoth._suite
