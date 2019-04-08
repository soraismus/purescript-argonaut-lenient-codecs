module Test.Main
  ( main
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Flex
  ( flexDecodeJson
  , flexDecodeJsonWith
  , flexDecodeJsonWithBoth
  )
import Data.Argonaut.Decode.Lenient (lenientDecodeJson)
import Data.Argonaut.Decode.Standard (decodeJsonWith, decodeJsonWith')
import Data.Argonaut.Decode.Per (decodeJsonPer)
import Data.Argonaut.Decode.X (xDecodeJsonWith)
import Data.Argonaut.Decode.XFlex (xFlexDecodeJsonWithBoth)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, isError, report, summarize)
import Data.Tuple (Tuple(Tuple), uncurry)
import Effect (Effect)
import Test.Unit (Test, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Type.Row (type (+), RProxy(RProxy))

type TypeRep_0 (f :: Type -> Type) r = ( a0 :: Int, a1 :: Int | r )
type TypeRep_1 f r = ( a2 :: f Int | r )
type TypeRep_2 f r = ( a3 :: f String, a4 :: f Boolean | r )
type TypeRep_3 f r = ( | (TypeRep_0 f) + (TypeRep_1 f) + r )
type TypeRep_4 f r = ( | (TypeRep_1 f) + (TypeRep_2 f) + r )
type TypeRep_5 f r = ( | (TypeRep_0 f) + (TypeRep_1 f) + (TypeRep_2 f) + r )

r_ :: RProxy ()
r_ = RProxy

r_1 :: RProxy (TypeRep_1 Maybe ())
r_1 = RProxy

r_4 :: RProxy (TypeRep_4 Maybe ())
r_4 = RProxy

type Type_0 = { | TypeRep_0 Maybe () }
type Type_1 = { | TypeRep_3 Maybe () }
type Type_2 = { | TypeRep_5 Maybe () }
type Type_3 = { | TypeRep_4 Maybe () }
type Type_4 = { | TypeRep_1 Maybe () }

type Type_5 = { | TypeRep_0 Array () }
type Type_6 = { | TypeRep_3 Array () }
type Type_7 = { | TypeRep_5 Array () }
type Type_8 = { | TypeRep_4 Array () }
type Type_9 = { | TypeRep_1 Array () }

val0 :: Type_0
val0 = { a0: 0, a1: 1 }

val1 :: Type_1
val1 = { a0: 0, a1: 1, a2: Just 2 }

val2 :: Type_2
val2 = { a0: 0, a1: 1, a2: Just 2, a3: Just "hello", a4: Just true }

val3 :: Type_5
val3 = { a0: 0, a1: 1 }

val4 :: Type_6
val4 = { a0: 0, a1: 1, a2: [2] }

val5 :: Type_7
val5 = { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] }

check
  :: forall f a
   . Status f
  => f a
  -> String
  -> (a -> Boolean)
  -> Tuple String Boolean
check result msg predicate =
  summarize
    (Tuple failsUnexpectedly false)
    (\val ->
      let
        state = predicate val
        msg' = if state then successful else msg
      in Tuple msg' state)
    result

check'
  :: forall f a
   . Status f
  => f a
  -> (a -> Boolean)
  -> Boolean
  -> String
  -> Tuple String Boolean
check' result predicate _ msg = check result msg predicate

fails :: forall f a . Status f => f a -> Tuple String Boolean
fails result =
  if isError result
    then Tuple successful true
    else Tuple doesntFail false

capriciousFailure :: String
capriciousFailure = "capricious failure"

notVal0 :: String
notVal0 = "isn't equal to val0"

notVal1 :: String
notVal1 = "isn't equal to val1"

notVal2 :: String
notVal2 = "isn't equal to val2"

notVal3 :: String
notVal3 = "isn't equal to val3"

notVal4 :: String
notVal4 = "isn't equal to val4"

notVal5 :: String
notVal5 = "isn't equal to val5"

doesntMeet :: String
doesntMeet = "doesn't meet expectations"

doesntFail :: String
doesntFail = "is decoded despite expectation of failure"

labels :: String
labels = "Tolerant labels: "

noTolerance :: String
noTolerance = "No Tolerance for Absent Fields"

successful :: String
successful = "successful test"

failsUnexpectedly :: String
failsUnexpectedly = "fails unexpectedly"

assert :: Tuple String Boolean -> Test
assert = uncurry Assert.assert

main :: Effect Unit
main = runTest do
  suite "Either String" do
    suite "Maybe" do

      suite "decodeJsonPer" do

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

      suite "lenientDecodeJson" do
        suite "Type_0" do
          test "val0" do
            let
              result :: Either String Type_0
              result = lenientDecodeJson (encodeJson val0)
            assert $ check' result (_ == val0) otherwise notVal0
          test "val1" do
            let
              result :: Either String Type_0
              result = lenientDecodeJson (encodeJson val1)
            assert $ check result doesntMeet
              (_ == { a0: val1.a0, a1: val1.a1 })
          test "val2" do
            let
              result :: Either String Type_0
              result = lenientDecodeJson (encodeJson val2)
            assert $ check result doesntMeet
              (_ == { a0: val2.a0, a1: val2.a1 })
        suite "Type_1" do
          test "val0" do
            let
              result :: Either String Type_1
              result = lenientDecodeJson (encodeJson val0)
            assert $ check result doesntMeet
              (_ == { a0: val0.a0, a1: val0.a1, a2: Nothing })
          test "val1" do
            let
              result :: Either String Type_1
              result = lenientDecodeJson (encodeJson val1)
            assert $ check' result (_ == val1) otherwise notVal1
          test "val2" do
            let
              result :: Either String Type_1
              result = lenientDecodeJson (encodeJson val2)
            assert $ check result doesntMeet
              (_ == { a0: val2.a0, a1: val2.a1, a2: val2.a2 })
        suite "Type_2" do
          test "val0" do
            let
              result :: Either String Type_2
              result = lenientDecodeJson (encodeJson val0)
            assert $ check result doesntMeet
              (_ == { a0: val0.a0
                    , a1: val0.a1
                    , a2: Nothing
                    , a3: Nothing
                    , a4: Nothing
                    })
          test "val1" do
            let
              result :: Either String Type_2
              result = lenientDecodeJson (encodeJson val1)
            assert $ check result doesntMeet
              (_ == { a0: val1.a0
                    , a1: val1.a1
                    , a2: val1.a2
                    , a3: Nothing
                    , a4: Nothing
                    })
          test "val2" do
            let
              result :: Either String Type_2
              result = lenientDecodeJson (encodeJson val2)
            assert $ check' result (_ == val2) otherwise notVal2
        suite "Type_3" do
          test "val0" do
            let
              result :: Either String Type_3
              result = lenientDecodeJson (encodeJson val0)
            assert $ check result doesntMeet
              (_ == { a2: Nothing, a3: Nothing, a4: Nothing })
          test "val1" do
            let
              result :: Either String Type_3
              result = lenientDecodeJson (encodeJson val1)
            assert $ check result doesntMeet
              (_ == { a2: val1.a2, a3: Nothing, a4: Nothing })
          test "val2" do
            let
              result :: Either String Type_3
              result = lenientDecodeJson (encodeJson val2)
            assert $ check result doesntMeet
              (_ == { a2: val2.a2, a3: val2.a3, a4: val2.a4 })
      suite "flexDecodeJson" do
        suite "Type_0" do
          test "val0" do
            let
              result :: Either String Type_0
              result = flexDecodeJson r_ (encodeJson val0)
            assert $ check' result (_ == val0) otherwise notVal0
          test "val1" do
            let
              result :: Either String Type_0
              result = flexDecodeJson r_ (encodeJson val1)
            assert $ check' result (_ == val0) otherwise notVal0
          test "val2" do
            let
              result :: Either String Type_0
              result = flexDecodeJson r_ (encodeJson val2)
            assert $ check' result (_ == val0) otherwise notVal0
        suite "Type_1" do
          suite noTolerance do
            test "val0" do
              let
                result :: Either String Type_1
                result = flexDecodeJson r_ (encodeJson val0)
              assert $ fails result
            test "val1" do
              let
                result :: Either String Type_1
                result = flexDecodeJson r_ (encodeJson val1)
              assert $ check' result (_ == val1) otherwise notVal1
            test "val2" do
              let
                result :: Either String Type_1
                result = flexDecodeJson r_ (encodeJson val2)
              assert $ check' result (_ == val1) otherwise notVal1
          suite (labels <> "a2") do
            test "val0" do
              let
                result :: Either String Type_1
                result = flexDecodeJson r_1 (encodeJson val0)
              assert $ check result doesntMeet
                (_ == { a0: val0.a0, a1: val0.a1, a2: Nothing })
            test "val1" do
              let
                result :: Either String Type_1
                result = flexDecodeJson r_1 (encodeJson val1)
              assert $ check' result (_ == val1) otherwise notVal1
            test "val2" do
              let
                result :: Either String Type_1
                result = flexDecodeJson r_1 (encodeJson val2)
              assert $ check' result (_ == val1) otherwise notVal1
        suite "Type_2" do
          suite noTolerance do
            test "val0" do
              let
                result :: Either String Type_2
                result = flexDecodeJson r_ (encodeJson val0)
              assert $ fails result
            test "val1" do
              let
                result :: Either String Type_2
                result = flexDecodeJson r_ (encodeJson val1)
              assert $ fails result
            test "val2" do
              let
                result :: Either String Type_2
                result = flexDecodeJson r_ (encodeJson val2)
              assert $ check' result (_ == val2) otherwise notVal2
          suite (labels <> "a2") do
            test "val0" do
              let
                result :: Either String Type_2
                result = flexDecodeJson r_1 (encodeJson val0)
              assert $ fails result
            test "val1" do
              let
                result :: Either String Type_2
                result = flexDecodeJson r_1 (encodeJson val1)
              assert $ fails result
            test "val2" do
              let
                result :: Either String Type_2
                result = flexDecodeJson r_1 (encodeJson val2)
              assert $ check' result (_ == val2) otherwise notVal2
          suite (labels <> "a2, a3, a4") do
            test "val0" do
              let
                result :: Either String Type_2
                result = flexDecodeJson r_4 (encodeJson val0)
              assert $ check result doesntMeet
                (_ == val2 { a2 = Nothing, a3 = Nothing, a4 = Nothing })
            test "val1" do
              let
                result :: Either String Type_2
                result = flexDecodeJson r_4 (encodeJson val1)
              assert $ check result doesntMeet
                (_ == val2 { a3 = Nothing, a4 = Nothing })
            test "val2" do
              let
                result :: Either String Type_2
                result = flexDecodeJson r_4 (encodeJson val2)
              assert $ check' result (_ == val2) otherwise notVal2
      suite "decodeJsonWith'" do
        suite "Type_4" do
          suite ("Override " <> "a2") do
            test "Override with Just" do
              let
                result :: Either String Type_4
                result =
                  decodeJsonWith'
                    { a2: \json -> Right $ Just 10 }
                    (encodeJson val1)
              assert $ check' result (_ == { a2: Just 10 }) otherwise doesntMeet
            test "Override with Nothing" do
              let
                result :: Either String Type_4
                result =
                  decodeJsonWith'
                    { a2: \json -> Right Nothing }
                    (encodeJson val1)
              assert $ check' result (_ == { a2: Nothing }) otherwise doesntMeet
            test "Override with Failure" do
              let
                result :: Either String Type_4
                result =
                  decodeJsonWith'
                    { a2: \json -> Left capriciousFailure }
                    (encodeJson val1)
              assert $ fails result
        suite "Type_1" do
          suite ("Override " <> "a2, a1, a2") do
            test "#0" do
              let
                result :: Either String Type_1
                result =
                  decodeJsonWith'
                    { a0: \json -> (Right 1000) :: Either String Int
                    , a1: \json -> (Right 1001) :: Either String Int
                    , a2: \json -> (Right $ Just 1002) :: Either String (Maybe Int)
                    }
                    (encodeJson val1)
              assert $ check result doesntMeet
                (_ == { a0: 1000, a1: 1001, a2: Just 1002 })
            test "#1" do
              let
                result :: Either String Type_1
                result =
                  decodeJsonWith'
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
              assert $ check' result (_ == val1) otherwise  doesntMeet
            test "#2" do
              let
                result :: Either String Type_1
                result =
                  decodeJsonWith'
                    { a0: \json -> Right 1000
                    , a1: \json -> Right 1001
                    , a2: \json -> Right $ Nothing
                    }
                  (encodeJson val1)
              assert $ check result doesntMeet
                (_ == { a0: 1000, a1: 1001, a2: Nothing })
            test "#3" do
              let
                result :: Either String Type_1
                result =
                  decodeJsonWith'
                    { a0: \json -> Right 1000
                    , a1: \json -> Left "Capricious failure"
                    , a2: \json -> Right $ Just 1002
                    }
                  (encodeJson val1)
              assert $ fails result
      suite "decodeJsonWith" do
        suite "Type_1" do
          suite ("Override " <> "a2") do
            suite "val1" do
              test "Override with Just" do
                let
                  result :: Either String Type_1
                  result =
                    decodeJsonWith
                      { a2: \json -> Right $ Just 1002 }
                      (encodeJson val1)
                assert $ check result doesntMeet (_ == val1 { a2 = Just 1002 })
        suite "Type_2" do
          suite ("Override " <> "a2, a3, a4") do
            test "val1" do
              let
                result :: Either String Type_2
                result =
                  decodeJsonWith
                    { a2: \json -> Right $ Just 1002
                    , a3: \json -> Right $ Just "bye"
                    , a4: \json -> Right $ Just false
                    }
                    (encodeJson val1)
              assert $ fails result
            test "val2" do
              let
                result :: Either String Type_2
                result =
                  decodeJsonWith
                    { a2: \json -> Right $ Just 1002
                    , a3: \json -> Right $ Just "bye"
                    , a4: \json -> Right $ Just false
                    }
                    (encodeJson val2)
              assert $ check result doesntMeet
                (_ == val2 { a2 = Just 1002, a3 = Just "bye", a4 = Just false })
          suite ("Override " <> "a1, a3") do
            suite "val2" do
              test "#0" do
                let
                  result :: Either String Type_2
                  result =
                    decodeJsonWith
                      { a1: \json -> Right $ 1002
                      , a3: \json -> Right $ Just "bye"
                      }
                    (encodeJson val2)
                assert $ check result doesntMeet
                  (_ == val2 { a1 = 1002, a3 = Just "bye" })
              test "#1" do
                let
                  result :: Either String Type_2
                  result =
                    decodeJsonWith
                      { a1: \json -> Right $ 1002
                      , a3: \json -> Left "Capricious failure"
                      }
                    (encodeJson val2)
                assert $ fails result
          suite ("Override " <> "a1, a4") do
            suite "val2" do
              test "#0" do
                let
                  result :: Either String Type_2
                  result =
                    decodeJsonWith
                      { a1: \json -> Right $ 1002
                      , a4: \json -> Right $ Just false
                      }
                    (encodeJson val2)
                assert $ check result doesntMeet
                  (_ == val2 { a1 = 1002, a4 = Just false })
          suite "Overriding no labels" do
            suite "val2" do
              test "#0" do
                let
                  result :: Either String Type_2
                  result = decodeJsonWith {} (encodeJson val2)
                assert $ check' result (_ == val2) otherwise doesntMeet
      suite "lenientDecodeJson" do
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
      suite "flexDecodeJsonWithBoth" do
        suite "Type_2" do
          suite "val0" do
            test "#0" do
              let
                result :: Either String Type_2
                result =
                  flexDecodeJsonWithBoth
                    { a0: \json -> Right 100
                    , a1: \json -> Right 101
                    }
                    { a2: \json -> Right $ Just 102
                    , a3: \json -> Right $ Just "bye"
                    , a4: \json -> Right $ Just false
                    }
                    (encodeJson val0)
              assert $ check result doesntMeet
                (_ == { a0: 100
                      , a1: 101
                      , a2: Nothing
                      , a3: Nothing
                      , a4: Nothing
                      })
            test "#1" do
              let
                result :: Either String Type_2
                result =
                  flexDecodeJsonWithBoth
                    {}
                    { a2: \json -> Right $ Just 102
                    , a4: \json -> Right $ Just false
                    }
                    (encodeJson val0)
              assert $ fails result
            test "#2" do
              let
                result :: Either String Type_2
                result =
                  flexDecodeJsonWithBoth
                    {}
                    { a2: \json -> Right $ Just 102 }
                    (encodeJson val0)
              assert $ fails result
            test "#3" do
              let
                result :: Either String Type_2
                result =
                  flexDecodeJsonWithBoth
                    { a4: \json -> Right $ Just false }
                    { a2: \json -> Right $ Just 102 }
                    (encodeJson val0)
              assert $ fails result
            suite "val1" do
              test "#0" do
                let
                  result :: Either String Type_2
                  result =
                    flexDecodeJsonWithBoth
                      { a0: \json -> Right 100
                      , a1: \json -> Right 101
                      }
                      { a2: \json -> Right $ Just 102
                      , a3: \json -> Right $ Just "bye"
                      , a4: \json -> Right $ Just false
                      }
                      (encodeJson val1)
                assert $ check result doesntMeet
                  (_ == { a0: 100
                        , a1: 101
                        , a2: Just 102
                        , a3: Nothing
                        , a4: Nothing
                        })
              test "#1" do
                let
                  result :: Either String Type_2
                  result =
                    flexDecodeJsonWithBoth
                      {}
                      { a2: \json -> Right $ Just 102
                      , a4: \json -> Right $ Just false
                      }
                      (encodeJson val1)
                assert $ fails result
              test "#2" do
                let
                  result :: Either String Type_2
                  result =
                    flexDecodeJsonWithBoth
                      {}
                      { a2: \json -> Right $ Just 102
                      , a3: \json -> Right $ Just "bye"
                      , a4: \json -> Right $ Just false
                      }
                      (encodeJson val1)
                assert $ check result doesntMeet
                  (_ == { a0: val1.a0
                        , a1: val1.a1
                        , a2: Just 102
                        , a3: Nothing
                        , a4: Nothing
                        })
            suite "val2" do
              test "#0" do
                let
                  result :: Either String Type_2
                  result =
                    flexDecodeJsonWithBoth
                      { a0: \json -> Right 100
                      , a1: \json -> Right 101
                      }
                      { a2: \json -> Right $ Just 102
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
                    flexDecodeJsonWithBoth
                      { a0: \json -> Right 100
                      , a1: \json -> Right 101
                      , a3: \json -> Right $ Just "bye"
                      }
                      { a2: \json -> Right $ Just 102
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
              test "#2" do
                let
                  result :: Either String Type_2
                  result =
                    flexDecodeJsonWithBoth
                      {}
                      { a2: \json -> Right $ Just 102
                      , a4: \json -> Right $ Just false
                      }
                      (encodeJson val2)
                assert $ check result doesntMeet
                  (_ == { a0: val2.a0
                        , a1: val2.a1
                        , a2: Just 102
                        , a3: val2.a3
                        , a4: Just false
                        })
      suite "xDecodeJsonWith" do
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
      suite "xDecodeJsonWithBoth" do
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
    suite "Array" do

      suite "decodeJsonPer" do

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

      suite "lenientDecodeJson" do
        suite "Type_5" do
          test "val3" do
            let
              result :: Either String Type_5
              result = lenientDecodeJson (encodeJson val3)
            assert $ check' result (_ == val3) otherwise notVal3
          test "val4" do
            let
              result :: Either String Type_5
              result = lenientDecodeJson (encodeJson val4)
            assert $ check result doesntMeet
              (_ == { a0: val4.a0, a1: val4.a1 })
          test "val5" do
            let
              result :: Either String Type_5
              result = lenientDecodeJson (encodeJson val5)
            assert $ check result doesntMeet
              (_ == { a0: val5.a0, a1: val5.a1 })
        suite "Type_6" do
          test "val3" do
            let
              result :: Either String Type_6
              result = lenientDecodeJson (encodeJson val3)
            assert $ check result doesntMeet
              (_ == { a0: val3.a0, a1: val3.a1, a2: [] })
          test "val4" do
            let
              result :: Either String Type_6
              result = lenientDecodeJson (encodeJson val4)
            assert $ check' result (_ == val4) otherwise notVal4
          test "val5" do
            let
              result :: Either String Type_6
              result = lenientDecodeJson (encodeJson val5)
            assert $ check result doesntMeet
              (_ == { a0: val5.a0, a1: val5.a1, a2: val5.a2 })
        suite "Type_7" do
          test "val3" do
            let
              result :: Either String Type_7
              result = lenientDecodeJson (encodeJson val3)
            assert $ check result doesntMeet
              (_ == { a0: val3.a0, a1: val3.a1, a2: [], a3: [], a4: [] })
          test "val4" do
            let
              result :: Either String Type_7
              result = lenientDecodeJson (encodeJson val4)
            assert $ check result doesntMeet
              (_ == { a0: val4.a0, a1: val4.a1, a2: val4.a2, a3: [], a4: [] })
          test "val5" do
            let
              result :: Either String Type_7
              result = lenientDecodeJson (encodeJson val5)
            assert $ check' result (_ == val5) otherwise notVal5
        suite "Type_8" do
          test "val3" do
            let
              result :: Either String Type_8
              result = lenientDecodeJson (encodeJson val3)
            assert $ check result doesntMeet
              (_ == { a2: [], a3: [], a4: [] })
          test "val4" do
            let
              result :: Either String Type_8
              result = lenientDecodeJson (encodeJson val4)
            assert $ check result doesntMeet
              (_ == { a2: val4.a2, a3: [], a4: [] })
          test "val5" do
            let
              result :: Either String Type_8
              result = lenientDecodeJson (encodeJson val5)
            assert $ check result doesntMeet
              (_ == { a2: val5.a2, a3: val5.a3, a4: val5.a4 })

      suite "flexDecodeJsonWithBoth" do
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
  suite "Maybe" do
    suite "Array" do
      suite "flexDecodeJsonWithBoth" do
        suite "Type_7" do
          suite "val4" do
            test "#0" do
              let
                result :: Either String Type_7
                result =
                  flexDecodeJsonWithBoth
                    { a0: \json -> report 100
                    , a1: \json -> report 101
                    , a3: \json -> report $ ["bye"]
                    }
                    { a2: \json -> report $ [102]
                    , a4: \json -> report $ [false]
                    }
                    (encodeJson val4)
              assert $ fails result
            test "#1" do
              let
                result :: Either String Type_7
                result =
                  flexDecodeJsonWithBoth
                    { a0: \json -> report 100
                    , a1: \json -> report 101
                    }
                    { a2: \json -> report $ [102]
                    , a3: \json -> report $ ["bye"]
                    , a4: \json -> report $ [false]
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
                    { a0: \json -> report 100
                    , a1: \json -> report 101
                    }
                    { a2: \json -> report $ [102]
                    , a3: \json -> report $ ["bye"]
                    , a4: \json -> report $ [false]
                    }
                    (encodeJson val5)
              assert $ check result doesntMeet
                (_ == { a0: 100
                      , a1: 101
                      , a2: [102]
                      , a3: ["bye"]
                      , a4: [false]
                      })
