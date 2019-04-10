module Test.Utils where

import Data.Maybe (Maybe(Just))
import Data.Status.Class (class Status, isError, summarize)
import Data.Tuple (Tuple(Tuple), uncurry)
import Test.Unit (Test)
import Test.Unit.Assert (assert) as Assert
import Type.Row (type (+), RProxy(RProxy))

type TypeRep_0 (f :: Type -> Type) r = ( a0 :: Int, a1 :: Int | r )
type TypeRep_1 f r = ( a2 :: f Int | r )
type TypeRep_2 f r = ( a3 :: f String, a4 :: f Boolean | r )
type TypeRep_3 f r = ( | (TypeRep_0 f) + (TypeRep_1 f) + r )
type TypeRep_4 f r = ( | (TypeRep_1 f) + (TypeRep_2 f) + r )
type TypeRep_5 f r = ( | (TypeRep_0 f) + (TypeRep_1 f) + (TypeRep_2 f) + r )

r_ :: RProxy ()
r_ = RProxy

r_1m :: RProxy (TypeRep_1 Maybe ())
r_1m = RProxy

r_4m :: RProxy (TypeRep_4 Maybe ())
r_4m = RProxy

r_1a :: RProxy (TypeRep_1 Array ())
r_1a = RProxy

r_4a :: RProxy (TypeRep_4 Array ())
r_4a = RProxy

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
