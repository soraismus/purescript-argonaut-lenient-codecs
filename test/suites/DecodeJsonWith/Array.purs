module Test.Suites.DecodeJsonWith.Array
  ( _suite
  ) where

import Prelude (discard, otherwise, ($), (==), (<>))

import Data.Argonaut.Decode.Standard (decodeJsonWith)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_6
  , Type_7
  , assert
  , check
  , check'
  , doesntMeet
  , fails
  , val4
  , val5
  )

_suite :: TestSuite
_suite =
  suite "Array" do
    suite "Type_6" do
      suite ("Override " <> "a2") do
        suite "val4" do
          test "Override with Just" do
            let
              result :: Either String Type_6
              result =
                decodeJsonWith
                  { a2: \json -> Right $ [1002] }
                  (encodeJson val4)
            assert $ check result doesntMeet (_ == val4 { a2 = [1002] })
    suite "Type_7" do
      suite ("Override " <> "a2, a3, a4") do
        test "val4" do
          let
            result :: Either String Type_7
            result =
              decodeJsonWith
                { a2: \json -> Right $ [1002]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val4)
          assert $ fails result
        test "val5" do
          let
            result :: Either String Type_7
            result =
              decodeJsonWith
                { a2: \json -> Right $ [1002]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == val5 { a2 = [1002], a3 = ["bye"], a4 = [false] })
      suite ("Override " <> "a1, a3") do
        suite "val5" do
          test "#0" do
            let
              result :: Either String Type_7
              result =
                decodeJsonWith
                  { a1: \json -> Right $ 1002
                  , a3: \json -> Right $ ["bye"]
                  }
                (encodeJson val5)
            assert $ check result doesntMeet
              (_ == val5 { a1 = 1002, a3 = ["bye"] })
          test "#1" do
            let
              result :: Either String Type_7
              result =
                decodeJsonWith
                  { a1: \json -> Right $ 1002
                  , a3: \json -> Left "Capricious failure"
                  }
                (encodeJson val5)
            assert $ fails result
      suite ("Override " <> "a1, a4") do
        suite "val5" do
          test "#0" do
            let
              result :: Either String Type_7
              result =
                decodeJsonWith
                  { a1: \json -> Right $ 1002
                  , a4: \json -> Right $ [false]
                  }
                (encodeJson val5)
            assert $ check result doesntMeet
              (_ == val5 { a1 = 1002, a4 = [false] })
      suite "Overriding no labels" do
        suite "val5" do
          test "#0" do
            let
              result :: Either String Type_7
              result = decodeJsonWith {} (encodeJson val5)
            assert $ check' result (_ == val5) otherwise doesntMeet

