module Test.Suites.XFlexDecodeJsonWithBoth.Maybe
  ( suitex
  ) where

import Prelude (($), (==))

import Data.Argonaut.Decode.XFlex (xFlexDecodeJsonWithBoth)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, check, withErrorMsg)

suitex :: TestSuite
suitex =
  suite "Maybe" do
    suite "xFlexDecodeJsonWithBoth" do
      suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int, a3 :: Maybe String, a4 :: Maybe Boolean }" do
        suite "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
          test "#0" do
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
                xFlexDecodeJsonWithBoth
                  { a0: \json rest -> Right 100
                  , a1: \json rest -> Right 101
                  }
                  { a2: \json (Tuple _ _) -> Right $ Just 102
                  , a3: \json (Tuple _ _) -> Right $ Just "bye"
                  , a4: \json (Tuple _ _) -> Right $ Just false
                  }
                  (encodeJson { a0: 0
                              , a1: 1
                              , a2: Just 2
                              , a3: Just "hello"
                              , a4: Just true
                              })
            assert $ check result withErrorMsg
              (_ == { a0: 100
                    , a1: 101
                    , a2: Just 102
                    , a3: Just "bye"
                    , a4: Just false
                    })
