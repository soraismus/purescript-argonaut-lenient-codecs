module Test.Suites.DecodeJsonWithPrime.Maybe
  ( suitex
  ) where

import Prelude (discard, ($), (==), (<>))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Standard (decodeJsonWith')
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, capriciousFailure, check, fails, withErrorMsg)

suitex :: TestSuite
suitex =
  suite "Maybe" do
    suite "{ a2 :: Maybe Int }" do
      suite ("Override " <> "a2") do
        test "Override with Just" do
          let
            result :: Either String { a2 :: Maybe Int }
            result =
              decodeJsonWith'
                { a2: \json -> Right $ Just 10 }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ check result withErrorMsg (_ == { a2: Just 10 })
        test "Override with Nothing" do
          let
            result :: Either String { a2 :: Maybe Int }
            result =
              decodeJsonWith'
                { a2: \json -> Right Nothing }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ check result withErrorMsg (_ == { a2: Nothing })
        test "Override with Failure" do
          let
            result :: Either String { a2 :: Maybe Int }
            result =
              decodeJsonWith'
                { a2: \json -> Left capriciousFailure }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ fails result
    suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int }" do
      suite ("Override " <> "a2, a1, a2") do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonWith'
                { a0: \json -> (Right 1000) :: Either String Int
                , a1: \json -> (Right 1001) :: Either String Int
                , a2: \json -> (Right $ Just 1002) :: Either String (Maybe Int)
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ check result withErrorMsg
            (_ == { a0: 1000, a1: 1001, a2: Just 1002 })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonWith'
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
                where
                decodeJsonInt :: Json -> Either String Int
                decodeJsonInt = decodeJson
                decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
                decodeJsonMaybeInt = decodeJson
          assert $ check result withErrorMsg (_ == { a0: 0, a1: 1, a2: Just 2 })
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonWith'
                { a0: \json -> Right 1000
                , a1: \json -> Right 1001
                , a2: \json -> Right $ Nothing
                }
              (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ check result withErrorMsg
            (_ == { a0: 1000, a1: 1001, a2: Nothing })
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonWith'
                { a0: \json -> Right 1000
                , a1: \json -> Left "Capricious failure"
                , a2: \json -> Right $ Just 1002
                }
              (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ fails result
