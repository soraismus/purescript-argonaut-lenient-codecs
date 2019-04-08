module Test.Suites.FlexDecodeJsonWithBoth.Array
  ( _suite
  ) where

import Prelude

import Data.Argonaut.Decode.Flex (flexDecodeJsonWithBoth)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_7
  , assert
  , check
  , doesntMeet
  , fails
  , val3
  , val4
  , val5
  )

_suite :: TestSuite
_suite =
  suite "Array" do
    suite "Type_7" do
      suite "val3" do
        test "#0" do
          let
            result :: Either String Type_7
            result =
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a3: \json -> Right $ ["bye"]
                }
                { a2: \json -> Right $ [102]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val3)
          assert $ fails result
        test "#1" do
          let
            result :: Either String Type_7
            result =
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                { a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val3)
          assert $ check result doesntMeet
            (_ == { a0: 100
                  , a1: 101
                  , a2: []
                  , a3: []
                  , a4: []
                  })
      suite "val4" do
        test "#0" do
          let
            result :: Either String Type_7
            result =
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a3: \json -> Right $ ["bye"]
                }
                { a2: \json -> Right $ [102]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val4)
          assert $ fails result
        test "#1" do
          let
            result :: Either String Type_7
            result =
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                { a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val4)
          assert $ check result doesntMeet
            (_ == { a0: 100
                  , a1: 101
                  , a2: [102]
                  , a3: []
                  , a4: []
                  })
      suite "val5" do
        test "#0" do
          let
            result :: Either String Type_7
            result =
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a3: \json -> Right $ ["bye"]
                }
                { a2: \json -> Right $ [102]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == { a0: 100
                  , a1: 101
                  , a2: [102]
                  , a3: ["bye"]
                  , a4: [false]
                  })
        test "#1" do
          let
            result :: Either String Type_7
            result =
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                { a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == { a0: 100
                  , a1: 101
                  , a2: [102]
                  , a3: ["bye"]
                  , a4: [false]
                  })
