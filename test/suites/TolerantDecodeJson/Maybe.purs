module Test.Suites.TolerantDecodeJson.Maybe
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Decode.Tolerant (tolerantDecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( assert
  , check
  , check'
  , notVal0
  , notVal1
  , notVal2
  , withErrorMsg
  )

shouldBeVal2 :: String
shouldBeVal2 = notVal2

suitex :: TestSuite
suitex =
  suite "Maybe" do
    suite "{ a0 :: Int, a1 :: Int }" do
      test "{ a0: 0, a1: 1 }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1 })
        assert $ check' result (_ == { a0: 0, a1: 1 }) otherwise notVal0
      test "{ a0: 0, a1: 1, a2: Just 2 }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1, a2: Just 2 })
        assert $ check result withErrorMsg
          (_ == { a0: 0, a1: 1 })
      test "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int }
          result =
            tolerantDecodeJson
              (encodeJson { a0: 0
                          , a1: 1
                          , a2: Just 2
                          , a3: Just "hello"
                          , a4: Just true
                          })
        assert $ check result withErrorMsg
          (_ == { a0: 0, a1: 1 })
    suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int }" do
      test "{ a0: 0, a1: 1 }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1 })
        assert $ check result withErrorMsg
          (_ == { a0: 0, a1: 1, a2: Nothing })
      test "{ a0: 0, a1: 1, a2: Just 2 }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1, a2: Just 2 })
        assert $
          check'
            result (_ == { a0: 0, a1: 1, a2: Just 2 })
            otherwise notVal1
      test "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
          result =
            tolerantDecodeJson
              (encodeJson { a0: 0
                          , a1: 1
                          , a2: Just 2
                          , a3: Just "hello"
                          , a4: Just true
                          })
        assert $ check result withErrorMsg
          (_ == { a0: 0, a1: 1, a2: Just 2 })
    suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int, a3 :: Maybe String, a4 :: Maybe Boolean }" do
      test "{ a0: 0, a1: 1 }" do
        let
          result
            :: Either
                String
                { a0 :: Int
                , a1 :: Int
                , a2 :: Maybe Int
                , a3 :: Maybe String
                , a4 :: Maybe Boolean
                }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1 })
        assert $ check result withErrorMsg
          (_ == { a0: 0
                , a1: 1
                , a2: Nothing
                , a3: Nothing
                , a4: Nothing
                })
      test "{ a0: 0, a1: 1, a2: Just 2 }" do
        let
          result
            :: Either
                String
                { a0 :: Int
                , a1 :: Int
                , a2 :: Maybe Int
                , a3 :: Maybe String
                , a4 :: Maybe Boolean
                }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1, a2: Just 2 })
        assert $ check result withErrorMsg
          (_ == { a0: 0
                , a1: 1
                , a2: Just 2
                , a3: Nothing
                , a4: Nothing
                })
      test "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
        let
          result
            :: Either
                String
                { a0 :: Int
                , a1 :: Int
                , a2 :: Maybe Int
                , a3 :: Maybe String
                , a4 :: Maybe Boolean
                }
          result =
            tolerantDecodeJson
              (encodeJson { a0: 0
                          , a1: 1
                          , a2: Just 2
                          , a3: Just "hello"
                          , a4: Just true
                          })
        assert $ check result shouldBeVal2
          (_ == { a0: 0, a1: 1, a2: Just 2, a3: Just "hello", a4: Just true })
    suite "{ a2 :: Maybe Int, a3 :: Maybe String, a4 :: Maybe Boolean }" do
      test "{ a0: 0, a1: 1 }" do
        let
          result
            :: Either
                String
                { a2 :: Maybe Int
                , a3 :: Maybe String
                , a4 :: Maybe Boolean
                }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1 })
        assert $ check result withErrorMsg
          (_ == { a2: Nothing, a3: Nothing, a4: Nothing })
      test "{ a0: 0, a1: 1, a2: Just 2 }" do
        let
          result
            :: Either
                String
                { a2 :: Maybe Int
                , a3 :: Maybe String
                , a4 :: Maybe Boolean
                }
          result = tolerantDecodeJson (encodeJson { a0: 0, a1: 1, a2: Just 2 })
        assert $ check result withErrorMsg
          (_ == { a2: Just 2, a3: Nothing, a4: Nothing })
      test "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
        let
          result
            :: Either
                String
                { a2 :: Maybe Int
                , a3 :: Maybe String
                , a4 :: Maybe Boolean
                }
          result =
            tolerantDecodeJson
              (encodeJson { a0: 0
                          , a1: 1
                          , a2: Just 2
                          , a3: Just "hello"
                          , a4: Just true
                          })
        assert $ check result withErrorMsg
          (_ == { a2: Just 2, a3: Just "hello", a4: Just true })
