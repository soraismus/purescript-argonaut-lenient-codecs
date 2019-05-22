module Test.Suites.XDecodeJsonWith.Array
  ( suitex
  ) where

import Prelude (discard, mod, show, ($), (==), (<$>))

import Data.Argonaut.Decode.X (xDecodeJsonWith)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_5
  , Type_7
  , assert
  , check
  , doesntMeet
  , val5
  )

suitex :: TestSuite
suitex =
  suite "Array" do
    suite "Type_7" do
      suite "val5" do
        test "#0" do
          let
            result :: Either String Type_7
            result =
              xDecodeJsonWith
                { a2: \json (rest :: Type_5) -> Right $ [1002]
                , a3: \json (rest :: Type_5) -> Right $ ["bye"]
                , a4: \json (rest :: Type_5) -> Right $ [false]
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == { a0: val5.a0
                  , a1: val5.a1
                  , a2: [1002]
                  , a3: ["bye"]
                  , a4: [false]
                  })
        test "#1" do
          let
            result :: Either String Type_7
            result =
              xDecodeJsonWith
                { a2: \json (rest :: Type_5) -> Right $ [rest.a0]
                , a3: \json (rest :: Type_5) -> Right $ ["bye"]
                , a4: \json (rest :: Type_5) -> Right $ [false]
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == { a0: val5.a0
                  , a1: val5.a1
                  , a2: [val5.a0]
                  , a3: ["bye"]
                  , a4: [false]
                  })
        test "#2" do
          let
            result :: Either String Type_7
            result =
              xDecodeJsonWith
                { a2: \json (rest :: Type_5) -> Right $ [rest.a0]
                , a3: \json rest -> Right $ [show rest.a0]
                , a4: \json rest -> Right $ [rest.a1 `mod` 2 == 0]
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == val5 { a2 = [val5.a0]
                      , a3 = [show val5.a0]
                      , a4 = [val5.a1 `mod` 2 == 0]
                      })
        test "#3" do
          let
            isEven :: Int -> Boolean
            isEven i = (i `mod` 2) == 0
            result :: Either String Type_7
            result =
              xDecodeJsonWith
                { a4: \json rest -> Right $ isEven <$> rest.a2 }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == val5 { a4 = isEven <$> val5.a2 })
