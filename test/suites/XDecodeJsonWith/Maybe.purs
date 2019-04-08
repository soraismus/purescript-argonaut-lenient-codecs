module Test.Suites.XDecodeJsonWith.Maybe
  ( _suite
  ) where

import Prelude (discard, mod, show, ($), (==), (<$>))

import Data.Argonaut.Decode.X (xDecodeJsonWith)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Just))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_0
  , Type_2
  , assert
  , check
  , doesntMeet
  , val2
  )

_suite :: TestSuite
_suite =
  suite "Maybe" do
    suite "Type_2" do
      suite "val2" do
        test "#0" do
          let
            result :: Either String Type_2
            result =
              xDecodeJsonWith
                { a2: \json (rest :: Type_0) -> Right $ Just 1002
                , a3: \json (rest :: Type_0) -> Right $ Just "bye"
                , a4: \json (rest :: Type_0) -> Right $ Just false
                }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == { a0: val2.a0
                  , a1: val2.a1
                  , a2: Just 1002
                  , a3: Just "bye"
                  , a4: Just false
                  })
        test "#1" do
          let
            result :: Either String Type_2
            result =
              xDecodeJsonWith
                { a2: \json (rest :: Type_0) -> Right $ Just (rest.a0)
                , a3: \json (rest :: Type_0) -> Right $ Just "bye"
                , a4: \json (rest :: Type_0) -> Right $ Just false
                }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == { a0: val2.a0
                  , a1: val2.a1
                  , a2: Just val2.a0
                  , a3: Just "bye"
                  , a4: Just false
                  })
        test "#2" do
          let
            result :: Either String Type_2
            result =
              xDecodeJsonWith
                { a2: \json (rest :: Type_0) -> Right $ Just (rest.a0)
                , a3: \json rest -> Right $ Just $ show rest.a0
                , a4: \json rest -> Right $ Just $ (rest.a1 `mod` 2 == 0)
                }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == val2 { a2 = Just val2.a0
                      , a3 = Just (show val2.a0)
                      , a4 = Just (val2.a1 `mod` 2 == 0)
                      })
        test "#3" do
          let
            isEven :: Int -> Boolean
            isEven i = (i `mod` 2) == 0
            result :: Either String Type_2
            result =
              xDecodeJsonWith
                { a4: \json rest -> Right $ isEven <$> rest.a2 }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == val2 { a4 = isEven <$> val2.a2 })
