module Test.Suites.DecodeJsonPer.Array
  ( suitex
  ) where

import Prelude (discard, otherwise, ($), (==))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Per (decodeJsonPer)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_5
  , Type_6
  , Type_7
  , assert
  , capriciousFailure
  , check
  , check'
  , doesntMeet
  , fails
  , notVal3
  , notVal4
  , notVal5
  , val3
  , val4
  , val5
  )

suitex :: TestSuite
suitex =
  suite "Array" do
    suite "Type_5" do
      suite "val3" do
        test "#0" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson val3)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101 })
        test "#1" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                }
                (encodeJson val3)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson val3)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson val3)
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check' result (_ == val3) otherwise notVal3
      suite "val4" do
        test "#0" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson val4)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101 })
        test "#1" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                }
                (encodeJson val4)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson val4)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson val4)
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check result doesntMeet
            (_ == { a0: val4.a0, a1: val4.a1 })
      suite "val5" do
        test "#0" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101 })
        test "#1" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                }
                (encodeJson val5)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson val5)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_5
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson val5)
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check result doesntMeet
            (_ == { a0: val5.a0, a1: val5.a1 })
    suite "Type_6" do
      suite "val3" do
        test "#0" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson val3)
          assert $ fails result
        test "#1" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson val3)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson val3)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                }
                (encodeJson val3)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonArrayInt :: Json -> Either String (Array Int)
              decodeJsonArrayInt = decodeJson
          assert $ fails result
        test "#4" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                }
                (encodeJson val3)
          assert $ fails result
      suite "val4" do
        test "#0" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson val4)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101, a2: [102] })
        test "#1" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson val4)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson val4)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                }
                (encodeJson val4)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonArrayInt :: Json -> Either String (Array Int)
              decodeJsonArrayInt = decodeJson
          assert $ check' result (_ == val4) otherwise notVal4
        test "#4" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                }
                (encodeJson val4)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101, a2: [] })
      suite "val5" do
        test "#0" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101, a2: [102] })
        test "#1" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson val5)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson val5)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                }
                (encodeJson val5)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonArrayInt :: Json -> Either String (Array Int)
              decodeJsonArrayInt = decodeJson
          assert $ check result doesntMeet
            (_ == { a0: val5.a0, a1: val5.a1, a2: val5.a2 })
        test "#4" do
          let
            result :: Either String Type_6
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101, a2: [] })
    suite "Type_7" do
      suite "val3" do
        test "#0" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val3)
          assert $ fails result
        test "#1" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val3)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val3)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                , a3: decodeJsonArrayString
                , a4: decodeJsonArrayBoolean
                }
                (encodeJson val3)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonArrayInt :: Json -> Either String (Array Int)
              decodeJsonArrayInt = decodeJson
              decodeJsonArrayString :: Json -> Either String (Array String)
              decodeJsonArrayString = decodeJson
              decodeJsonArrayBoolean :: Json -> Either String (Array Boolean)
              decodeJsonArrayBoolean = decodeJson
          assert $ fails result
        test "#4" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val3)
          assert $ fails result
      suite "val4" do
        test "#0" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val4)
          assert $ fails result
        test "#1" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val4)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val4)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                , a3: decodeJsonArrayString
                , a4: decodeJsonArrayBoolean
                }
                (encodeJson val4)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonArrayInt :: Json -> Either String (Array Int)
              decodeJsonArrayInt = decodeJson
              decodeJsonArrayString :: Json -> Either String (Array String)
              decodeJsonArrayString = decodeJson
              decodeJsonArrayBoolean :: Json -> Either String (Array Boolean)
              decodeJsonArrayBoolean = decodeJson
          assert $ fails result
        test "#4" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val4)
          assert $ fails result
      suite "val5" do
        test "#0" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
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
        test "#1" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val5)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val5)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                , a3: decodeJsonArrayString
                , a4: decodeJsonArrayBoolean
                }
                (encodeJson val5)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonArrayInt :: Json -> Either String (Array Int)
              decodeJsonArrayInt = decodeJson
              decodeJsonArrayString :: Json -> Either String (Array String)
              decodeJsonArrayString = decodeJson
              decodeJsonArrayBoolean :: Json -> Either String (Array Boolean)
              decodeJsonArrayBoolean = decodeJson
          assert $ check' result (_ == val5) otherwise notVal5
        test "#4" do
          let
            result :: Either String Type_7
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson val5)
          assert $ check result doesntMeet
            (_ == { a0: 100
                  , a1: 101
                  , a2: []
                  , a3: ["bye"]
                  , a4: [false]
                  })
