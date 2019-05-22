module Test.Suites.DecodeJsonWith.Array
  ( suitex
  ) where

import Prelude (discard, ($), (==), (<>))

import Data.Argonaut.Decode.Standard (decodeJsonWith)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, check, fails, withErrorMsg)

suitex :: TestSuite
suitex =
  suite "Array" do
    suite "{ a0 :: Int, a1 :: Int, a2 :: Array Int }" do
      suite ("Override " <> "a2") do
        suite "{ a0: 0, a1: 1, a2: [2] }" do
          test "Override with Just" do
            let
              result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
              result =
                decodeJsonWith
                  { a2: \json -> Right $ [1002] }
                  (encodeJson { a0: 0, a1: 1, a2: [2] })
            assert $ check result withErrorMsg (_ == { a0: 0, a1: 1, a2: [1002] })
    suite "{ a0 :: Int, a1 :: Int, a2 :: Array Int, a3 :: Array String, a4 :: Array Boolean }" do
      suite ("Override " <> "a2, a3, a4") do
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
            result =
              decodeJsonWith
                { a2: \json -> Right $ [1002]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
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
              decodeJsonWith
                { a2: \json -> Right $ [1002]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: [1002], a3: ["bye"], a4: [false] })
      suite ("Override " <> "a1, a3") do
        suite "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
          test "#0" do
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
                decodeJsonWith
                  { a1: \json -> Right $ 1001
                  , a3: \json -> Right $ ["bye"]
                  }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
            assert $ check result withErrorMsg
              (_ == { a0: 0, a1: 1001, a2: [2], a3: ["bye"], a4: [true] })
          test "#1" do
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
                decodeJsonWith
                  { a1: \json -> Right $ 1002
                  , a3: \json -> Left "Capricious failure"
                  }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
            assert $ fails result
      suite ("Override " <> "a1, a4") do
        suite "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
          test "#0" do
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
                decodeJsonWith
                  { a1: \json -> Right $ 1001
                  , a4: \json -> Right $ [false]
                  }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
            assert $ check result withErrorMsg
              (_ == { a0: 0, a1: 1001, a2: [2], a3: ["hello"], a4: [false] })
      suite "Overriding no labels" do
        suite "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
          test "#0" do
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
                decodeJsonWith
                  {}
                  (encodeJson { a0: 0
                              , a1: 1
                              , a2: [2]
                              , a3: ["hello"]
                              , a4: [true]
                              })
            assert $ check result withErrorMsg
              (_ == { a0: 0
                    , a1: 1
                    , a2: [2]
                    , a3: ["hello"]
                    , a4: [true]
                    })

