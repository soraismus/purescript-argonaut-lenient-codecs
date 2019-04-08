module Test.Suites.LenientDecodeJson.Array
  ( _suite
  ) where

import Prelude

import Data.Argonaut.Decode.Lenient (lenientDecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_5
  , Type_6
  , Type_7
  , Type_8
  , assert
  , check
  , check'
  , doesntMeet
  , notVal3
  , notVal4
  , notVal5
  , val3
  , val4
  , val5
  )

_suite :: TestSuite
_suite =
  suite "Array" do
    suite "Type_5" do
      test "val3" do
        let
          result :: Either String Type_5
          result = lenientDecodeJson (encodeJson val3)
        assert $ check' result (_ == val3) otherwise notVal3
      test "val4" do
        let
          result :: Either String Type_5
          result = lenientDecodeJson (encodeJson val4)
        assert $ check result doesntMeet
          (_ == { a0: val4.a0, a1: val4.a1 })
      test "val5" do
        let
          result :: Either String Type_5
          result = lenientDecodeJson (encodeJson val5)
        assert $ check result doesntMeet
          (_ == { a0: val5.a0, a1: val5.a1 })
    suite "Type_6" do
      test "val3" do
        let
          result :: Either String Type_6
          result = lenientDecodeJson (encodeJson val3)
        assert $ check result doesntMeet
          (_ == { a0: val3.a0, a1: val3.a1, a2: [] })
      test "val4" do
        let
          result :: Either String Type_6
          result = lenientDecodeJson (encodeJson val4)
        assert $ check' result (_ == val4) otherwise notVal4
      test "val5" do
        let
          result :: Either String Type_6
          result = lenientDecodeJson (encodeJson val5)
        assert $ check result doesntMeet
          (_ == { a0: val5.a0, a1: val5.a1, a2: val5.a2 })
    suite "Type_7" do
      test "val3" do
        let
          result :: Either String Type_7
          result = lenientDecodeJson (encodeJson val3)
        assert $ check result doesntMeet
          (_ == { a0: val3.a0, a1: val3.a1, a2: [], a3: [], a4: [] })
      test "val4" do
        let
          result :: Either String Type_7
          result = lenientDecodeJson (encodeJson val4)
        assert $ check result doesntMeet
          (_ == { a0: val4.a0, a1: val4.a1, a2: val4.a2, a3: [], a4: [] })
      test "val5" do
        let
          result :: Either String Type_7
          result = lenientDecodeJson (encodeJson val5)
        assert $ check' result (_ == val5) otherwise notVal5
    suite "Type_8" do
      test "val3" do
        let
          result :: Either String Type_8
          result = lenientDecodeJson (encodeJson val3)
        assert $ check result doesntMeet
          (_ == { a2: [], a3: [], a4: [] })
      test "val4" do
        let
          result :: Either String Type_8
          result = lenientDecodeJson (encodeJson val4)
        assert $ check result doesntMeet
          (_ == { a2: val4.a2, a3: [], a4: [] })
      test "val5" do
        let
          result :: Either String Type_8
          result = lenientDecodeJson (encodeJson val5)
        assert $ check result doesntMeet
          (_ == { a2: val5.a2, a3: val5.a3, a4: val5.a4 })
