module Test.Suites.DecodeJsonWith'.Array
  ( _suite
  ) where

import Prelude (discard, otherwise, ($), (==), (<>))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Standard (decodeJsonWith')
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_6
  , Type_9
  , assert
  , capriciousFailure
  , check
  , check'
  , doesntMeet
  , fails
  , val4
  )

_suite :: TestSuite
_suite =
  suite "Array" do
    suite "Type_9" do
      suite ("Override " <> "a2") do
        test "Override with singleton array" do
          let
            result :: Either String Type_9
            result =
              decodeJsonWith'
                { a2: \json -> Right $ [10] }
                (encodeJson val4)
          assert $ check' result (_ == { a2: [10] }) otherwise doesntMeet
        test "Override with empty array" do
          let
            result :: Either String Type_9
            result =
              decodeJsonWith'
                { a2: \json -> Right [] }
                (encodeJson val4)
          assert $ check' result (_ == { a2: [] }) otherwise doesntMeet
        test "Override with Failure" do
          let
            result :: Either String Type_9
            result =
              decodeJsonWith'
                { a2: \json -> Left capriciousFailure }
                (encodeJson val4)
          assert $ fails result
    suite "Type_6" do
      suite ("Override " <> "a2, a1, a2") do
        test "#0" do
          let
            result :: Either String Type_6
            result =
              decodeJsonWith'
                { a0: \json -> (Right 1000) :: Either String Int
                , a1: \json -> (Right 1001) :: Either String Int
                , a2: \json -> (Right $ [1002]) :: Either String (Array Int)
                }
                (encodeJson val4)
          assert $ check result doesntMeet
            (_ == { a0: 1000, a1: 1001, a2: [1002] })
        test "#1" do
          let
            result :: Either String Type_6
            result =
              decodeJsonWith'
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                }
                (encodeJson val4)
                where
                decodeJsonInt :: Json -> Either String Int
                decodeJsonInt = decodeJson
                decodeJsonArrayInt :: Json -> Either String (Array Int)
                decodeJsonArrayInt = decodeJson
          assert $ check' result (_ == val4) otherwise  doesntMeet
        test "#2" do
          let
            result :: Either String Type_6
            result =
              decodeJsonWith'
                { a0: \json -> Right 1000
                , a1: \json -> Right 1001
                , a2: \json -> Right $ []
                }
              (encodeJson val4)
          assert $ check result doesntMeet
            (_ == { a0: 1000, a1: 1001, a2: [] })
        test "#3" do
          let
            result :: Either String Type_6
            result =
              decodeJsonWith'
                { a0: \json -> Right 1000
                , a1: \json -> Left "Capricious failure"
                , a2: \json -> Right $ [1002]
                }
              (encodeJson val4)
          assert $ fails result
