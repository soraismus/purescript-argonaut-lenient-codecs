module Test.Suites.FlexDecodeJsonWith.Array
  ( _suite
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Flex (flexDecodeJsonWith)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_6
  , Type_7
  , assert
  , check
  , check'
  , doesntMeet
  , val3
  , val4
  , val5
  )

_suite :: TestSuite
_suite =
  suite "Array" do
    suite "Type_6" do
      suite "val3" do
        test "#0" do
          let
            result :: Either String Type_6
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002] }
                (encodeJson val3)
          assert $ check result doesntMeet
            (_ == { a0: val3.a0, a1: val3.a1, a2: [] })
      suite "val4" do
        test "#0" do
          let
            result :: Either String Type_6
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002] }
                (encodeJson val4)
          assert $ check result doesntMeet
            (_ == { a0: val4.a0, a1: val4.a1, a2: [1002] })
      suite "val5" do
        test "#0" do
          let
            result :: Either String Type_6
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002] }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == { a0: val5.a0, a1: val5.a1, a2: [1002] })
        test "#1" do
          let
            result :: Either String Type_6
            result =
              flexDecodeJsonWith
                { a2: decodeJson' }
                (encodeJson val5)
              where
              decodeJson' :: Json -> Either String (Array Int)
              decodeJson' = decodeJson
          assert $ check result doesntMeet
            (_ == { a0: val5.a0, a1: val5.a1, a2: val5.a2 })
    suite "Type_7" do
      suite "val3" do
        test "#0" do
          let
            result :: Either String Type_7
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val3)
          assert $ check result doesntMeet
            (_ == { a0: val3.a0
                  , a1: val3.a1
                  , a2: []
                  , a3: []
                  , a4: []
                  })
      suite "val4" do
        test "#0" do
          let
            result :: Either String Type_7
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val4)
          assert $ check result doesntMeet
            (_ == { a0: val3.a0
                  , a1: val3.a1
                  , a2: [1002]
                  , a3: []
                  , a4: []
                  })
      suite "val5" do
        test "#0" do
          let
            result :: Either String Type_7
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ [1002]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == { a0: val3.a0
                  , a1: val3.a1
                  , a2: [1002]
                  , a3: ["bye"]
                  , a4: [false]
                  })
        test "#1" do
          let
            result :: Either String Type_7
            result =
              flexDecodeJsonWith
                { a2: decodeJson2
                , a3: decodeJson3
                , a4: decodeJson4
                }
                (encodeJson val5)
                where
                decodeJson2 :: Json -> Either String (Array Int)
                decodeJson2 = decodeJson
                decodeJson3 :: Json -> Either String (Array String)
                decodeJson3 = decodeJson
                decodeJson4 :: Json -> Either String (Array Boolean)
                decodeJson4 = decodeJson
          assert $ check' result (_ == val5) otherwise doesntMeet
