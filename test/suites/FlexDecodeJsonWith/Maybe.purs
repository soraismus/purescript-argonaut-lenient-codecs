module Test.Suites.FlexDecodeJsonWith.Maybe
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Flex (flexDecodeJsonWith)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Just, Nothing))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_1
  , Type_2
  , assert
  , check
  , check'
  , doesntMeet
  , val0
  , val1
  , val2
  )

suitex :: TestSuite
suitex =
  suite "Maybe" do
    suite "Type_1" do
      suite "val0" do
        test "#0" do
          let
            result :: Either String Type_1
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002 }
                (encodeJson val0)
          assert $ check result doesntMeet
            (_ == { a0: val0.a0, a1: val0.a1, a2: Nothing })
      suite "val1" do
        test "#0" do
          let
            result :: Either String Type_1
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002 }
                (encodeJson val1)
          assert $ check result doesntMeet
            (_ == { a0: val1.a0, a1: val1.a1, a2: Just 1002 })
      suite "val2" do
        test "#0" do
          let
            result :: Either String Type_1
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002 }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == { a0: val2.a0, a1: val2.a1, a2: Just 1002 })
        test "#1" do
          let
            result :: Either String Type_1
            result =
              flexDecodeJsonWith
                { a2: decodeJson' }
                (encodeJson val2)
              where
              decodeJson' :: Json -> Either String (Maybe Int)
              decodeJson' = decodeJson
          assert $ check result doesntMeet
            (_ == { a0: val2.a0, a1: val2.a1, a2: val2.a2 })
    suite "Type_2" do
      suite "val0" do
        test "#0" do
          let
            result :: Either String Type_2
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val0)
          assert $ check result doesntMeet
            (_ == { a0: val0.a0
                  , a1: val0.a1
                  , a2: Nothing
                  , a3: Nothing
                  , a4: Nothing
                  })
      suite "val1" do
        test "#0" do
          let
            result :: Either String Type_2
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val1)
          assert $ check result doesntMeet
            (_ == { a0: val0.a0
                  , a1: val0.a1
                  , a2: Just 1002
                  , a3: Nothing
                  , a4: Nothing
                  })
      suite "val2" do
        test "#0" do
          let
            result :: Either String Type_2
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == { a0: val0.a0
                  , a1: val0.a1
                  , a2: Just 1002
                  , a3: Just "bye"
                  , a4: Just false
                  })
        test "#1" do
          let
            result :: Either String Type_2
            result =
              flexDecodeJsonWith
                { a2: decodeJson2
                , a3: decodeJson3
                , a4: decodeJson4
                }
                (encodeJson val2)
                where
                decodeJson2 :: Json -> Either String (Maybe Int)
                decodeJson2 = decodeJson
                decodeJson3 :: Json -> Either String (Maybe String)
                decodeJson3 = decodeJson
                decodeJson4 :: Json -> Either String (Maybe Boolean)
                decodeJson4 = decodeJson
          assert $ check' result (_ == val2) otherwise doesntMeet
