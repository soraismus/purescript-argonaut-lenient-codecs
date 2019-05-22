module Test.Suites.FlexDecodeJson.Maybe
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Decode.Flex (flexDecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Type.Row (RProxy(RProxy))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( assert
  , check
  , check'
  , fails
  , labels
  , noTolerance
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
          result =
            flexDecodeJson
              (RProxy :: RProxy ())
              (encodeJson { a0: 0, a1: 1 })
        assert $ check' result (_ == { a0: 0, a1: 1 }) otherwise notVal0
      test "{ a0: 0, a1: 1, a2: Just 2 }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int }
          result =
            flexDecodeJson
              (RProxy :: RProxy ())
              (encodeJson { a0: 0, a1: 1, a2: Just 2 })
        assert $ check' result (_ == { a0: 0, a1: 1 }) otherwise notVal0
      test "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
        let
          result :: Either String { a0 :: Int, a1 :: Int }
          result =
            flexDecodeJson
              (RProxy :: RProxy ())
              (encodeJson { a0: 0
                          , a1: 1
                          , a2: Just 2
                          , a3: Just "hello"
                          , a4: Just true
                          })
        assert $ check' result (_ == { a0: 0, a1: 1 }) otherwise notVal0
    suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int }" do
      suite noTolerance do
        test "{ a0: 0, a1: 1 }" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              flexDecodeJson
                (RProxy :: RProxy ())
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
        test "{ a0: 0, a1: 1, a2: Just 2 }" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              flexDecodeJson
                (RProxy :: RProxy ())
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $
            check'
              result (_ == { a0: 0, a1: 1, a2: Just 2 })
              otherwise notVal1
        test "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              flexDecodeJson
                (RProxy :: RProxy ())
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $
            check'
              result (_ == { a0: 0, a1: 1, a2: Just 2 })
              otherwise notVal1
      suite (labels <> "a2") do
        test "{ a0: 0, a1: 1 }" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              flexDecodeJson
                (RProxy :: RProxy (a2 :: Maybe Int))
                (encodeJson { a0: 0, a1: 1 })
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: Nothing })
        test "{ a0: 0, a1: 1, a2: Just 2 }" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              flexDecodeJson
                (RProxy :: RProxy (a2 :: Maybe Int))
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $
            check'
              result (_ == { a0: 0, a1: 1, a2: Just 2 })
              otherwise notVal1
        test "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              flexDecodeJson
              (RProxy :: RProxy (a2 :: Maybe Int))
              (encodeJson { a0: 0
                          , a1: 1
                          , a2: Just 2
                          , a3: Just "hello"
                          , a4: Just true
                          })
          assert $
            check'
              result (_ == { a0: 0, a1: 1, a2: Just 2 })
              otherwise notVal1
    suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int, a3 :: Maybe String, a4 :: Maybe Boolean }" do
      suite noTolerance do
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
            result =
              flexDecodeJson
                (RProxy :: RProxy ())
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
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
            result =
              flexDecodeJson
                (RProxy :: RProxy ())
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ fails result
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
              flexDecodeJson
                (RProxy :: RProxy ())
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ check result shouldBeVal2
            (_ == { a0: 0, a1: 1, a2: Just 2, a3: Just "hello", a4: Just true })
      suite (labels <> "a2") do
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
            result =
              flexDecodeJson
                (RProxy :: RProxy (a2 :: Maybe Int))
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
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
            result =
              flexDecodeJson
                (RProxy :: RProxy (a2 :: Maybe Int))
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ fails result
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
              flexDecodeJson
                (RProxy :: RProxy (a2 :: Maybe Int))
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ check result shouldBeVal2
            (_ == { a0: 0, a1: 1, a2: Just 2, a3: Just "hello", a4: Just true })
      suite (labels <> "a2, a3, a4") do
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
            result =
              flexDecodeJson
                (RProxy :: RProxy (a2 :: Maybe Int, a3 :: Maybe String, a4 :: Maybe Boolean))
                (encodeJson { a0: 0, a1: 1 })
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: Nothing, a3: Nothing, a4: Nothing })
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
            result =
              flexDecodeJson
                (RProxy :: RProxy (a2 :: Maybe Int, a3 :: Maybe String, a4 :: Maybe Boolean))
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: Just 2, a3: Nothing, a4: Nothing })
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
              flexDecodeJson
                (RProxy :: RProxy (a2 :: Maybe Int, a3 :: Maybe String, a4 :: Maybe Boolean))
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ check result shouldBeVal2
            (_ == { a0: 0, a1: 1, a2: Just 2, a3: Just "hello", a4: Just true })
