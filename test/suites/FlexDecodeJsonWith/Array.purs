module Test.Suites.FlexDecodeJsonWith.Array
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Flex (flexDecodeJsonWith)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, check, withErrorMsg)

suitex :: TestSuite
suitex =
  suite "Array" do
    suite "{ a0 :: Int, a1 :: Int, a2 :: Array Int }" do
      suite "{ a0: 0, a1: 1 }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002] }
                (encodeJson { a0: 0, a1: 1 })
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: [] })
      suite "{ a0: 0, a1: 1, a2: [2] }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002] }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: [1002] })
      suite "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002] }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: [1002] })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              flexDecodeJsonWith
                { a2: decodeJson' }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
              where
              decodeJson' :: Json -> Either String (Array Int)
              decodeJson' = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: [2] })
    suite "{ a0 :: Int, a1 :: Int, a2 :: Array Int, a3 :: Array String, a4 :: Array Boolean }" do
      suite "{ a0: 0, a1: 1 }" do
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
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ check result withErrorMsg
            (_ == { a0: 0
                  , a1: 1
                  , a2: []
                  , a3: []
                  , a4: []
                  })
      suite "{ a0: 0, a1: 1, a2: [2] }" do
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
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ check result withErrorMsg
            (_ == { a0: 0
                  , a1: 1
                  , a2: [1002]
                  , a3: []
                  , a4: []
                  })
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
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 0
                  , a1: 1
                  , a2: [1002]
                  , a3: ["bye"]
                  , a4: [false]
                  })
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
              flexDecodeJsonWith
                { a2: decodeJson2
                , a3: decodeJson3
                , a4: decodeJson4
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
                where
                decodeJson2 :: Json -> Either String (Array Int)
                decodeJson2 = decodeJson
                decodeJson3 :: Json -> Either String (Array String)
                decodeJson3 = decodeJson
                decodeJson4 :: Json -> Either String (Array Boolean)
                decodeJson4 = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
