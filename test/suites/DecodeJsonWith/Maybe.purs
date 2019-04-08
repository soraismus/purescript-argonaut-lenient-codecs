module Test.Suites.DecodeJsonWith.Maybe
  ( _suite
  ) where

import Prelude (discard, otherwise, ($), (==), (<>))

import Data.Argonaut.Decode.Standard (decodeJsonWith)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_1
  , Type_2
  , assert
  , check
  , check'
  , doesntMeet
  , fails
  , val1
  , val2
  )

_suite :: TestSuite
_suite =
  suite "Maybe" do
    suite "Type_1" do
      suite ("Override " <> "a2") do
        suite "val1" do
          test "Override with Just" do
            let
              result :: Either String Type_1
              result =
                decodeJsonWith
                  { a2: \json -> Right $ Just 1002 }
                  (encodeJson val1)
            assert $ check result doesntMeet (_ == val1 { a2 = Just 1002 })
    suite "Type_2" do
      suite ("Override " <> "a2, a3, a4") do
        test "val1" do
          let
            result :: Either String Type_2
            result =
              decodeJsonWith
                { a2: \json -> Right $ Just 1002
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val1)
          assert $ fails result
        test "val2" do
          let
            result :: Either String Type_2
            result =
              decodeJsonWith
                { a2: \json -> Right $ Just 1002
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == val2 { a2 = Just 1002, a3 = Just "bye", a4 = Just false })
      suite ("Override " <> "a1, a3") do
        suite "val2" do
          test "#0" do
            let
              result :: Either String Type_2
              result =
                decodeJsonWith
                  { a1: \json -> Right $ 1002
                  , a3: \json -> Right $ Just "bye"
                  }
                (encodeJson val2)
            assert $ check result doesntMeet
              (_ == val2 { a1 = 1002, a3 = Just "bye" })
          test "#1" do
            let
              result :: Either String Type_2
              result =
                decodeJsonWith
                  { a1: \json -> Right $ 1002
                  , a3: \json -> Left "Capricious failure"
                  }
                (encodeJson val2)
            assert $ fails result
      suite ("Override " <> "a1, a4") do
        suite "val2" do
          test "#0" do
            let
              result :: Either String Type_2
              result =
                decodeJsonWith
                  { a1: \json -> Right $ 1002
                  , a4: \json -> Right $ Just false
                  }
                (encodeJson val2)
            assert $ check result doesntMeet
              (_ == val2 { a1 = 1002, a4 = Just false })
      suite "Overriding no labels" do
        suite "val2" do
          test "#0" do
            let
              result :: Either String Type_2
              result = decodeJsonWith {} (encodeJson val2)
            assert $ check' result (_ == val2) otherwise doesntMeet
