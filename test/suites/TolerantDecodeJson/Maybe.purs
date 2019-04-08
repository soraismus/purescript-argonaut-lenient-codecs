module Test.Suites.TolerantDecodeJson.Maybe
  ( _suite
  ) where

import Prelude

import Data.Argonaut.Decode.Tolerant (tolerantDecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(Nothing))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_0
  , Type_1
  , Type_2
  , Type_3
  , assert
  , check
  , check'
  , doesntMeet
  , notVal0
  , notVal1
  , notVal2
  , val0
  , val1
  , val2
  )

_suite :: TestSuite
_suite =
  suite "Maybe" do
    suite "Type_0" do
      test "val0" do
        let
          result :: Either String Type_0
          result = tolerantDecodeJson (encodeJson val0)
        assert $ check' result (_ == val0) otherwise notVal0
      test "val1" do
        let
          result :: Either String Type_0
          result = tolerantDecodeJson (encodeJson val1)
        assert $ check result doesntMeet
          (_ == { a0: val1.a0, a1: val1.a1 })
      test "val2" do
        let
          result :: Either String Type_0
          result = tolerantDecodeJson (encodeJson val2)
        assert $ check result doesntMeet
          (_ == { a0: val2.a0, a1: val2.a1 })
    suite "Type_1" do
      test "val0" do
        let
          result :: Either String Type_1
          result = tolerantDecodeJson (encodeJson val0)
        assert $ check result doesntMeet
          (_ == { a0: val0.a0, a1: val0.a1, a2: Nothing })
      test "val1" do
        let
          result :: Either String Type_1
          result = tolerantDecodeJson (encodeJson val1)
        assert $ check' result (_ == val1) otherwise notVal1
      test "val2" do
        let
          result :: Either String Type_1
          result = tolerantDecodeJson (encodeJson val2)
        assert $ check result doesntMeet
          (_ == { a0: val2.a0, a1: val2.a1, a2: val2.a2 })
    suite "Type_2" do
      test "val0" do
        let
          result :: Either String Type_2
          result = tolerantDecodeJson (encodeJson val0)
        assert $ check result doesntMeet
          (_ == { a0: val0.a0
                , a1: val0.a1
                , a2: Nothing
                , a3: Nothing
                , a4: Nothing
                })
      test "val1" do
        let
          result :: Either String Type_2
          result = tolerantDecodeJson (encodeJson val1)
        assert $ check result doesntMeet
          (_ == { a0: val1.a0
                , a1: val1.a1
                , a2: val1.a2
                , a3: Nothing
                , a4: Nothing
                })
      test "val2" do
        let
          result :: Either String Type_2
          result = tolerantDecodeJson (encodeJson val2)
        assert $ check' result (_ == val2) otherwise notVal2
    suite "Type_3" do
      test "val0" do
        let
          result :: Either String Type_3
          result = tolerantDecodeJson (encodeJson val0)
        assert $ check result doesntMeet
          (_ == { a2: Nothing, a3: Nothing, a4: Nothing })
      test "val1" do
        let
          result :: Either String Type_3
          result = tolerantDecodeJson (encodeJson val1)
        assert $ check result doesntMeet
          (_ == { a2: val1.a2, a3: Nothing, a4: Nothing })
      test "val2" do
        let
          result :: Either String Type_3
          result = tolerantDecodeJson (encodeJson val2)
        assert $ check result doesntMeet
          (_ == { a2: val2.a2, a3: val2.a3, a4: val2.a4 })
