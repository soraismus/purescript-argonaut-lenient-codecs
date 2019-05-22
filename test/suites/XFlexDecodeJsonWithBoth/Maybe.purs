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
import Test.Utils
  ( Type_2
  , assert
  , check
  , doesntMeet
  , val2
  )

suitex :: TestSuite
suitex =
  suite "Maybe" do
    suite "xFlexDecodeJsonWithBoth" do
      suite "Type_2" do
        suite "val2" do
          test "#0" do
            let
              result :: Either String Type_2
              result =
                xFlexDecodeJsonWithBoth
                  { a0: \json rest -> Right 100
                  , a1: \json rest -> Right 101
                  }
                  { a2: \json (Tuple _ _) -> Right $ Just 102
                  , a3: \json (Tuple _ _) -> Right $ Just "bye"
                  , a4: \json (Tuple _ _) -> Right $ Just false
                  }
                  (encodeJson val2)
            assert $ check result doesntMeet
              (_ == { a0: 100
                    , a1: 101
                    , a2: Just 102
                    , a3: Just "bye"
                    , a4: Just false
                    })
