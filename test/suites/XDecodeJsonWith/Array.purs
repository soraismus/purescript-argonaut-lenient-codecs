module Test.Suites.XDecodeJsonWith.Array
  ( suitex
  ) where

import Prelude (discard, mod, show, ($), (==), (<$>))

import Data.Argonaut.Decode.X (xDecodeJsonWith)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, check, withErrorMsg)

suitex :: TestSuite
suitex =
  suite "Array" do
    suite "{ a0 :: Int, a1 :: Int, a2 :: Array Int, a3 :: Array String, a4 :: Array Boolean }" do
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
              xDecodeJsonWith
                { a2: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ [1002]
                , a3: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ ["bye"]
                , a4: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ [false]
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
              xDecodeJsonWith
                { a2: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ [rest.a0]
                , a3: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ ["bye"]
                , a4: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 0
                  , a1: 1
                  , a2: [0]
                  , a3: ["bye"]
                  , a4: [false]
                  })
        test "#2" do
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
              xDecodeJsonWith
                { a2: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ [rest.a0]
                , a3: \json rest -> Right $ [show rest.a0]
                , a4: \json rest -> Right $ [rest.a1 `mod` 2 == 0]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 0
                  , a1: 1
                  , a2: [0]
                  , a3: [show 0]
                  , a4: [1 `mod` 2 == 0]
                  })
        test "#3" do
          let
            isEven :: Int -> Boolean
            isEven i = (i `mod` 2) == 0
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
              xDecodeJsonWith
                { a4: \json rest -> Right $ isEven <$> rest.a2 }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 0
                  , a1: 1
                  , a2: [2]
                  , a3: ["hello"]
                  , a4: isEven <$> [2]
                  })
