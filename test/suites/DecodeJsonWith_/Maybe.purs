module Test.Suites.DecodeJsonWithPrime.Maybe
  ( suitex
  ) where

import Prelude (discard, otherwise, ($), (==), (<>))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Standard (decodeJsonWith')
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_1
  , Type_4
  , assert
  , capriciousFailure
  , check
  , check'
  , doesntMeet
  , fails
  , val1
  )

suitex :: TestSuite
suitex =
  suite "Maybe" do
    suite "Type_4" do
      suite ("Override " <> "a2") do
        test "Override with Just" do
          let
            result :: Either String Type_4
            result =
              decodeJsonWith'
                { a2: \json -> Right $ Just 10 }
                (encodeJson val1)
          assert $ check' result (_ == { a2: Just 10 }) otherwise doesntMeet
        test "Override with Nothing" do
          let
            result :: Either String Type_4
            result =
              decodeJsonWith'
                { a2: \json -> Right Nothing }
                (encodeJson val1)
          assert $ check' result (_ == { a2: Nothing }) otherwise doesntMeet
        test "Override with Failure" do
          let
            result :: Either String Type_4
            result =
              decodeJsonWith'
                { a2: \json -> Left capriciousFailure }
                (encodeJson val1)
          assert $ fails result
    suite "Type_1" do
      suite ("Override " <> "a2, a1, a2") do
        test "#0" do
          let
            result :: Either String Type_1
            result =
              decodeJsonWith'
                { a0: \json -> (Right 1000) :: Either String Int
                , a1: \json -> (Right 1001) :: Either String Int
                , a2: \json -> (Right $ Just 1002) :: Either String (Maybe Int)
                }
                (encodeJson val1)
          assert $ check result doesntMeet
            (_ == { a0: 1000, a1: 1001, a2: Just 1002 })
        test "#1" do
          let
            result :: Either String Type_1
            result =
              decodeJsonWith'
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                }
                (encodeJson val1)
                where
                decodeJsonInt :: Json -> Either String Int
                decodeJsonInt = decodeJson
                decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
                decodeJsonMaybeInt = decodeJson
          assert $ check' result (_ == val1) otherwise  doesntMeet
        test "#2" do
          let
            result :: Either String Type_1
            result =
              decodeJsonWith'
                { a0: \json -> Right 1000
                , a1: \json -> Right 1001
                , a2: \json -> Right $ Nothing
                }
              (encodeJson val1)
          assert $ check result doesntMeet
            (_ == { a0: 1000, a1: 1001, a2: Nothing })
        test "#3" do
          let
            result :: Either String Type_1
            result =
              decodeJsonWith'
                { a0: \json -> Right 1000
                , a1: \json -> Left "Capricious failure"
                , a2: \json -> Right $ Just 1002
                }
              (encodeJson val1)
          assert $ fails result
