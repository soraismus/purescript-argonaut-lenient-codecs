module Test.Suites.TolerantDecodeJson.Array
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Decode.Tolerant (tolerantDecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( assert
  , check
  , check'
  , notVal3
  , notVal4
  , notVal5
  , withErrorMsg
  )

suitex :: TestSuite
suitex =
  suite "Array" do
    suite "{ a0 :: Int, a1 :: Int }" do
      test "{ a0: 0, a1: 1 }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1 })
        assert $ check' result (_ == { a0: 0, a1: 1 }) otherwise notVal3
      test "{ a0: 0, a1: 1, a2: [2] }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1, a2: [2] })
        assert $ check result withErrorMsg
          (_ == { a0: 0, a1: 1 })
      test "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int }
          result =
            tolerantDecodeJson
              (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
        assert $ check result withErrorMsg
          (_ == { a0: 0, a1: 1 })
    suite "{ a0 :: Int, a1 :: Int, a2 :: Array Int }" do
      test "{ a0: 0, a1: 1 }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1 })
        assert $ check result withErrorMsg
          (_ == { a0: 0, a1: 1, a2: [] })
      test "{ a0: 0, a1: 1, a2: [2] }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1, a2: [2] })
        assert $ check' result (_ == { a0: 0, a1: 1, a2: [2] }) otherwise notVal4
      test "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
          result =
            tolerantDecodeJson
              (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
        assert $ check result withErrorMsg
          (_ == { a0: 0, a1: 1, a2: [2] })
    suite "{ a0 :: Int, a1 :: Int, a2 :: Array Int, a3 :: Array String, a4 :: Array Boolean }" do
      test "{ a0: 0, a1: 1 }" do
        let
          result
            :: Either
                String
                { a0 :: Int
                , a1 :: Int
                , a2 :: Array Int
                , a3 :: Array String
                , a4 :: Array Boolean
                }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1 })
        assert $ check result withErrorMsg
          (_ == { a0: 0, a1: 1, a2: [], a3: [], a4: [] })
      test "{ a0: 0, a1: 1, a2: [2] }" do
        let
          result
            :: Either
                String
                { a0 :: Int
                , a1 :: Int
                , a2 :: Array Int
                , a3 :: Array String
                , a4 :: Array Boolean
                }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1, a2: [2] })
        assert $ check result withErrorMsg
          (_ == { a0: 0, a1: 1, a2: [2], a3: [], a4: [] })
      test "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
        let
          result
            :: Either
                String
                { a0 :: Int
                , a1 :: Int
                , a2 :: Array Int
                , a3 :: Array String
                , a4 :: Array Boolean
                }
          result =
            tolerantDecodeJson
              (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
        assert $
          check'
            result (_ == { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
            otherwise notVal5
    suite "{ a2 :: Array Int, a3 :: Array String, a4 :: Array Boolean }" do
      test "{ a0: 0, a1: 1 }" do
        let
          result
            :: Either
                String
                { a2 :: Array Int
                , a3 :: Array String
                , a4 :: Array Boolean
                }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1 })
        assert $ check result withErrorMsg
          (_ == { a2: [], a3: [], a4: [] })
      test "{ a0: 0, a1: 1, a2: [2] }" do
        let
          result
            :: Either
                String
                { a2 :: Array Int
                , a3 :: Array String
                , a4 :: Array Boolean
                }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1, a2: [2] })
        assert $ check result withErrorMsg
          (_ == { a2: [2], a3: [], a4: [] })
      test "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
        let
          result
            :: Either
                String
                { a2 :: Array Int
                , a3 :: Array String
                , a4 :: Array Boolean
                }
          result =
            tolerantDecodeJson
              (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
        assert $ check result withErrorMsg
          (_ == { a2: [2], a3: ["hello"], a4: [true] })
