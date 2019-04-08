module Test.Suites.XFlexDecodeJsonWithBoth.Array
  ( _suite
  ) where

import Prelude (($), (==))

import Data.Argonaut.Decode.XFlex (xFlexDecodeJsonWithBoth)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Data.Tuple (Tuple(Tuple))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_7
  , assert
  , check
  , doesntMeet
  , val5
  )

_suite :: TestSuite
_suite =
  suite "Array" do
    suite "xFlexDecodeJsonWithBoth" do
      suite "Type_7" do
        suite "val5" do
          test "#0" do
            let
              result :: Either String Type_7
              result =
                xFlexDecodeJsonWithBoth
                  { a0: \json rest -> Right 100
                  , a1: \json rest -> Right 101
                  }
                  { a2: \json (Tuple _ _) -> Right $ [102]
                  , a3: \json (Tuple _ _) -> Right $ ["bye"]
                  , a4: \json (Tuple _ _) -> Right $ [false]
                  }
                  (encodeJson val5)
            assert $ check result doesntMeet
              (_ == { a0: 100
                    , a1: 101
                    , a2: [102]
                    , a3: ["bye"]
                    , a4: [false]
                    })
