module Test.Suites.FlexDecodeJson.Maybe
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Decode.Flex (flexDecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(Nothing))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_0
  , Type_1
  , Type_2
  , assert
  , check
  , check'
  , doesntMeet
  , fails
  , labels
  , noTolerance
  , notVal0
  , notVal1
  , notVal2
  , r_
  , r_1m
  , r_4m
  , val0
  , val1
  , val2
  )

suitex :: TestSuite
suitex =
  suite "Maybe" do
    suite "Type_0" do
      test "val0" do
        let
          result :: Either String Type_0
          result = flexDecodeJson r_ (encodeJson val0)
        assert $ check' result (_ == val0) otherwise notVal0
      test "val1" do
        let
          result :: Either String Type_0
          result = flexDecodeJson r_ (encodeJson val1)
        assert $ check' result (_ == val0) otherwise notVal0
      test "val2" do
        let
          result :: Either String Type_0
          result = flexDecodeJson r_ (encodeJson val2)
        assert $ check' result (_ == val0) otherwise notVal0
    suite "Type_1" do
      suite noTolerance do
        test "val0" do
          let
            result :: Either String Type_1
            result = flexDecodeJson r_ (encodeJson val0)
          assert $ fails result
        test "val1" do
          let
            result :: Either String Type_1
            result = flexDecodeJson r_ (encodeJson val1)
          assert $ check' result (_ == val1) otherwise notVal1
        test "val2" do
          let
            result :: Either String Type_1
            result = flexDecodeJson r_ (encodeJson val2)
          assert $ check' result (_ == val1) otherwise notVal1
      suite (labels <> "a2") do
        test "val0" do
          let
            result :: Either String Type_1
            result = flexDecodeJson r_1m (encodeJson val0)
          assert $ check result doesntMeet
            (_ == { a0: val0.a0, a1: val0.a1, a2: Nothing })
        test "val1" do
          let
            result :: Either String Type_1
            result = flexDecodeJson r_1m (encodeJson val1)
          assert $ check' result (_ == val1) otherwise notVal1
        test "val2" do
          let
            result :: Either String Type_1
            result = flexDecodeJson r_1m (encodeJson val2)
          assert $ check' result (_ == val1) otherwise notVal1
    suite "Type_2" do
      suite noTolerance do
        test "val0" do
          let
            result :: Either String Type_2
            result = flexDecodeJson r_ (encodeJson val0)
          assert $ fails result
        test "val1" do
          let
            result :: Either String Type_2
            result = flexDecodeJson r_ (encodeJson val1)
          assert $ fails result
        test "val2" do
          let
            result :: Either String Type_2
            result = flexDecodeJson r_ (encodeJson val2)
          assert $ check' result (_ == val2) otherwise notVal2
      suite (labels <> "a2") do
        test "val0" do
          let
            result :: Either String Type_2
            result = flexDecodeJson r_1m (encodeJson val0)
          assert $ fails result
        test "val1" do
          let
            result :: Either String Type_2
            result = flexDecodeJson r_1m (encodeJson val1)
          assert $ fails result
        test "val2" do
          let
            result :: Either String Type_2
            result = flexDecodeJson r_1m (encodeJson val2)
          assert $ check' result (_ == val2) otherwise notVal2
      suite (labels <> "a2, a3, a4") do
        test "val0" do
          let
            result :: Either String Type_2
            result = flexDecodeJson r_4m (encodeJson val0)
          assert $ check result doesntMeet
            (_ == val2 { a2 = Nothing, a3 = Nothing, a4 = Nothing })
        test "val1" do
          let
            result :: Either String Type_2
            result = flexDecodeJson r_4m (encodeJson val1)
          assert $ check result doesntMeet
            (_ == val2 { a3 = Nothing, a4 = Nothing })
        test "val2" do
          let
            result :: Either String Type_2
            result = flexDecodeJson r_4m (encodeJson val2)
          assert $ check' result (_ == val2) otherwise notVal2
