module Test.Suites.FlexDecodeJsonWithBoth.Array
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Decode.Flex (flexDecodeJsonWithBoth)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, check, fails, withErrorMsg)

suitex :: TestSuite
suitex =
  suite "Array" do
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
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a3: \json -> Right $ ["bye"]
                }
                { a2: \json -> Right $ [102]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
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
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                { a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ check result withErrorMsg
            (_ == { a0: 100
                  , a1: 101
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
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a3: \json -> Right $ ["bye"]
                }
                { a2: \json -> Right $ [102]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
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
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                { a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ check result withErrorMsg
            (_ == { a0: 100
                  , a1: 101
                  , a2: [102]
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
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a3: \json -> Right $ ["bye"]
                }
                { a2: \json -> Right $ [102]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 100
                  , a1: 101
                  , a2: [102]
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
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                { a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 100
                  , a1: 101
                  , a2: [102]
                  , a3: ["bye"]
                  , a4: [false]
                  })
