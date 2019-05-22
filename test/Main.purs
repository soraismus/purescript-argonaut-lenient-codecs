module Test.Main
  ( main
  ) where

import Prelude (Unit, discard)

import Effect (Effect)
import Test.Suites.DecodeJsonPer (suitex) as DecodeJsonPer
import Test.Suites.DecodeJsonWith (suitex) as DecodeJsonWith
import Test.Suites.DecodeJsonWithPrime (suitex) as DecodeJsonWith'
import Test.Suites.FlexDecodeJson (suitex) as FlexDecodeJson
import Test.Suites.FlexDecodeJsonWith (suitex) as FlexDecodeJsonWith
import Test.Suites.FlexDecodeJsonWithBoth (suitex) as FlexDecodeJsonWithBoth
import Test.Suites.TolerantDecodeJson (suitex) as TolerantDecodeJson
import Test.Suites.XDecodeJsonWith (suitex) as XDecodeJsonWith
import Test.Suites.XFlexDecodeJsonWithBoth (suitex) as XFlexDecodeJsonWithBoth
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  DecodeJsonPer.suitex
  DecodeJsonWith.suitex
  DecodeJsonWith'.suitex
  FlexDecodeJson.suitex
  FlexDecodeJsonWith.suitex
  FlexDecodeJsonWithBoth.suitex
  TolerantDecodeJson.suitex
  XDecodeJsonWith.suitex
  XFlexDecodeJsonWithBoth.suitex
