module Test.Main
  ( main
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Flex
  ( flexDecodeJsonWith
  , flexDecodeJsonWithBoth
  )
import Data.Argonaut.Decode.X (xDecodeJsonWith)
import Data.Argonaut.Decode.XFlex (xFlexDecodeJsonWithBoth)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, isError, report, summarize)
import Data.Tuple (Tuple(Tuple), uncurry)
import Effect (Effect)
import Test.Suites.DecodeJsonPer (_suite) as DecodeJsonPer
import Test.Suites.DecodeJsonWith (_suite) as DecodeJsonWith
import Test.Suites.DecodeJsonWith' (_suite) as DecodeJsonWith'
import Test.Suites.FlexDecodeJson (_suite) as FlexDecodeJson
import Test.Suites.LenientDecodeJson (_suite) as LenientDecodeJson
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
  suite "MODULES" do
    DecodeJsonPer._suite
    DecodeJsonWith._suite
    DecodeJsonWith'._suite
    LenientDecodeJson._suite
    FlexDecodeJson._suite

  suite "Either String" do
    suite "Maybe" do

      suite "flexDecodeJsonWith" do
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
