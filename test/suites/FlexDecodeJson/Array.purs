module Test.Suites.FlexDecodeJson.Array
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Decode.Flex (flexDecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_5
  , Type_6
  , Type_7
  , assert
  , check
  , check'
  , doesntMeet
  , fails
  , labels
  , noTolerance
  , notVal3
  , notVal4
  , notVal5
  , r_
  , r_1a
  , r_4a
  , val3
  , val4
  , val5
  )

suitex :: TestSuite
suitex =
  suite "Array" do
    suite "Type_5" do
      test "val3" do
        let
          result :: Either String Type_5
          result = flexDecodeJson r_ (encodeJson val3)
        assert $ check' result (_ == val3) otherwise notVal3
      test "val4" do
        let
          result :: Either String Type_5
          result = flexDecodeJson r_ (encodeJson val4)
        assert $ check' result (_ == val3) otherwise notVal3
      test "val5" do
        let
          result :: Either String Type_5
          result = flexDecodeJson r_ (encodeJson val5)
        assert $ check' result (_ == val3) otherwise notVal3
    suite "Type_6" do
      suite noTolerance do
        test "val3" do
          let
            result :: Either String Type_6
            result = flexDecodeJson r_ (encodeJson val3)
          assert $ fails result
        test "val4" do
          let
            result :: Either String Type_6
            result = flexDecodeJson r_ (encodeJson val4)
          assert $ check' result (_ == val4) otherwise notVal4
        test "val5" do
          let
            result :: Either String Type_6
            result = flexDecodeJson r_ (encodeJson val5)
          assert $ check' result (_ == val4) otherwise notVal4
      suite (labels <> "a2") do
        test "val3" do
          let
            result :: Either String Type_6
            result = flexDecodeJson r_1a (encodeJson val3)
          assert $ check result doesntMeet
            (_ == { a0: val3.a0, a1: val3.a1, a2: [] })
        test "val4" do
          let
            result :: Either String Type_6
            result = flexDecodeJson r_1a (encodeJson val4)
          assert $ check' result (_ == val4) otherwise notVal4
        test "val5" do
          let
            result :: Either String Type_6
            result = flexDecodeJson r_1a (encodeJson val5)
          assert $ check' result (_ == val4) otherwise notVal4
    suite "Type_7" do
      suite noTolerance do
        test "val3" do
          let
            result :: Either String Type_7
            result = flexDecodeJson r_ (encodeJson val3)
          assert $ fails result
        test "val4" do
          let
            result :: Either String Type_7
            result = flexDecodeJson r_ (encodeJson val4)
          assert $ fails result
        test "val5" do
          let
            result :: Either String Type_7
            result = flexDecodeJson r_ (encodeJson val5)
          assert $ check' result (_ == val5) otherwise notVal5
      suite (labels <> "a2") do
        test "val3" do
          let
            result :: Either String Type_7
            result = flexDecodeJson r_1a (encodeJson val3)
          assert $ fails result
        test "val4" do
          let
            result :: Either String Type_7
            result = flexDecodeJson r_1a (encodeJson val4)
          assert $ fails result
        test "val5" do
          let
            result :: Either String Type_7
            result = flexDecodeJson r_1a (encodeJson val5)
          assert $ check' result (_ == val5) otherwise notVal5
      suite (labels <> "a2, a3, a4") do
        test "val3" do
          let
            result :: Either String Type_7
            result = flexDecodeJson r_4a (encodeJson val3)
          assert $ check result doesntMeet
            (_ == val5 { a2 = [], a3 = [], a4 = [] })
        test "val4" do
          let
            result :: Either String Type_7
            result = flexDecodeJson r_4a (encodeJson val4)
          assert $ check result doesntMeet
            (_ == val5 { a3 = [], a4 = [] })
        test "val5" do
          let
            result :: Either String Type_7
            result = flexDecodeJson r_4a (encodeJson val5)
          assert $ check' result (_ == val5) otherwise notVal5
