module Test.Suites.DecodeJsonWithPrime.Array
  ( suitex
  ) where

import Prelude (discard, ($), (==), (<>))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Standard (decodeJsonWith')
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, capriciousFailure, check, fails, withErrorMsg)

suitex :: TestSuite
suitex =
  suite "Array" do
    suite "{ a2 :: Array Int }" do
      suite ("Override " <> "a2") do
        test "Override with singleton array" do
          let
            result :: Either String { a2 :: Array Int }
            result =
              decodeJsonWith'
                { a2: \json -> Right $ [10] }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ check result withErrorMsg (_ == { a2: [10] })
        test "Override with empty array" do
          let
            result :: Either String { a2 :: Array Int }
            result =
              decodeJsonWith'
                { a2: \json -> Right [] }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ check result withErrorMsg (_ == { a2: [] })
        test "Override with Failure" do
          let
            result :: Either String { a2 :: Array Int }
            result =
              decodeJsonWith'
                { a2: \json -> Left capriciousFailure }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
    suite "{ a0 :: Int, a1 :: Int, a2 :: Array Int }" do
      suite ("Override " <> "a2, a1, a2") do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonWith'
                { a0: \json -> (Right 1000) :: Either String Int
                , a1: \json -> (Right 1001) :: Either String Int
                , a2: \json -> (Right $ [1002]) :: Either String (Array Int)
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ check result withErrorMsg
            (_ == { a0: 1000, a1: 1001, a2: [1002] })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonWith'
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
                where
                decodeJsonInt :: Json -> Either String Int
                decodeJsonInt = decodeJson
                decodeJsonArrayInt :: Json -> Either String (Array Int)
                decodeJsonArrayInt = decodeJson
          assert $ check result withErrorMsg (_ == { a0: 0, a1: 1, a2: [2] })
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonWith'
                { a0: \json -> Right 1000
                , a1: \json -> Right 1001
                , a2: \json -> Right $ []
                }
              (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ check result withErrorMsg
            (_ == { a0: 1000, a1: 1001, a2: [] })
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonWith'
                { a0: \json -> Right 1000
                , a1: \json -> Left "Capricious failure"
                , a2: \json -> Right $ [1002]
                }
              (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
