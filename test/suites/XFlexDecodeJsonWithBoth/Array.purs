module Test.Suites.XFlexDecodeJsonWithBoth.Array
  ( suitex
  ) where

import Prelude (($), (==))

import Data.Argonaut.Decode.XFlex (xFlexDecodeJsonWithBoth)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Data.Tuple (Tuple(Tuple))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, check, withErrorMsg)

suitex :: TestSuite
suitex =
  suite "Array" do
    suite "xFlexDecodeJsonWithBoth" do
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
                xFlexDecodeJsonWithBoth
                  { a0: \json rest -> Right 100
                  , a1: \json rest -> Right 101
                  }
                  { a2: \json (Tuple _ _) -> Right $ [102]
                  , a3: \json (Tuple _ _) -> Right $ ["bye"]
                  , a4: \json (Tuple _ _) -> Right $ [false]
                  }
                  (encodeJson { a0: 0
                              , a1: 1
                              , a2: [2]
                              , a3: ["hello"]
                              , a4: [true]
                              })
            assert $ check result withErrorMsg
              (_ == { a0: 100
                    , a1: 101
                    , a2: [102]
                    , a3: ["bye"]
                    , a4: [false]
                    })
