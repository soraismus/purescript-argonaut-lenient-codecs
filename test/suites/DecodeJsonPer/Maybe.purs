module Test.Suites.DecodeJsonPer.Maybe
  ( _suite
  ) where

import Prelude (discard, otherwise, ($), (==))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Per (decodeJsonPer)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_0
  , Type_1
  , Type_2
  , assert
  , capriciousFailure
  , check
  , check'
  , doesntMeet
  , fails
  , notVal0
  , notVal1
  , notVal2
  , val0
  , val1
  , val2
  )

_suite :: TestSuite
_suite =
  suite "Maybe" do
    suite "Type_0" do
      suite "val0" do
        test "#0" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson val0)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101 })
        test "#1" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                }
                (encodeJson val0)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson val0)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson val0)
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check' result (_ == val0) otherwise notVal0
      suite "val1" do
        test "#0" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson val1)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101 })
        test "#1" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                }
                (encodeJson val1)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson val1)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson val1)
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check result doesntMeet
            (_ == { a0: val1.a0, a1: val1.a1 })
      suite "val2" do
        test "#0" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101 })
        test "#1" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                }
                (encodeJson val2)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson val2)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_0
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson val2)
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check result doesntMeet
            (_ == { a0: val2.a0, a1: val2.a1 })
    suite "Type_1" do
      suite "val0" do
        test "#0" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson val0)
          assert $ fails result
        test "#1" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson val0)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson val0)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                }
                (encodeJson val0)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
              decodeJsonMaybeInt = decodeJson
          assert $ fails result
        test "#4" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                }
                (encodeJson val0)
          assert $ fails result
      suite "val1" do
        test "#0" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson val1)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101, a2: Just 102 })
        test "#1" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson val1)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson val1)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                }
                (encodeJson val1)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
              decodeJsonMaybeInt = decodeJson
          assert $ check' result (_ == val1) otherwise notVal1
        test "#4" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                }
                (encodeJson val1)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101, a2: Nothing })
      suite "val2" do
        test "#0" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101, a2: Just 102 })
        test "#1" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson val2)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson val2)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                }
                (encodeJson val2)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
              decodeJsonMaybeInt = decodeJson
          assert $ check result doesntMeet
            (_ == { a0: val2.a0, a1: val2.a1, a2: val2.a2 })
        test "#4" do
          let
            result :: Either String Type_1
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == { a0: 100, a1: 101, a2: Nothing })
    suite "Type_2" do
      suite "val0" do
        test "#0" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val0)
          assert $ fails result
        test "#1" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val0)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val0)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                , a3: decodeJsonMaybeString
                , a4: decodeJsonMaybeBoolean
                }
                (encodeJson val0)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
              decodeJsonMaybeInt = decodeJson
              decodeJsonMaybeString :: Json -> Either String (Maybe String)
              decodeJsonMaybeString = decodeJson
              decodeJsonMaybeBoolean :: Json -> Either String (Maybe Boolean)
              decodeJsonMaybeBoolean = decodeJson
          assert $ fails result
        test "#4" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val0)
          assert $ fails result
      suite "val1" do
        test "#0" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val1)
          assert $ fails result
        test "#1" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val1)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val1)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                , a3: decodeJsonMaybeString
                , a4: decodeJsonMaybeBoolean
                }
                (encodeJson val1)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
              decodeJsonMaybeInt = decodeJson
              decodeJsonMaybeString :: Json -> Either String (Maybe String)
              decodeJsonMaybeString = decodeJson
              decodeJsonMaybeBoolean :: Json -> Either String (Maybe Boolean)
              decodeJsonMaybeBoolean = decodeJson
          assert $ fails result
        test "#4" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val1)
          assert $ fails result
      suite "val2" do
        test "#0" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == { a0: 100
                  , a1: 101
                  , a2: Just 102
                  , a3: Just "bye"
                  , a4: Just false
                  })
        test "#1" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val2)
          assert $ fails result
        test "#2" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val2)
          assert $ fails result
        test "#3" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                , a3: decodeJsonMaybeString
                , a4: decodeJsonMaybeBoolean
                }
                (encodeJson val2)
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
              decodeJsonMaybeInt = decodeJson
              decodeJsonMaybeString :: Json -> Either String (Maybe String)
              decodeJsonMaybeString = decodeJson
              decodeJsonMaybeBoolean :: Json -> Either String (Maybe Boolean)
              decodeJsonMaybeBoolean = decodeJson
          assert $ check' result (_ == val2) otherwise notVal2
        test "#4" do
          let
            result :: Either String Type_2
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson val2)
          assert $ check result doesntMeet
            (_ == { a0: 100
                  , a1: 101
                  , a2: Nothing
                  , a3: Just "bye"
                  , a4: Just false
                  })
