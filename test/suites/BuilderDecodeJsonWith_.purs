module Test.Suites.BuilderDecodeJsonWithPrime
  ( _suite
  ) where

import Prelude (discard, otherwise, ($), (==), (<>))

import Data.Argonaut.Decode.Standard.Builder (builderDecodeJsonWith')
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just))
import Record.Builder (Builder)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Utils
  ( Type_0
  , Type_1
  , Type_2
  , TypeRep_0
  , assert
  , check
  , check'
  , doesntMeet
  , fails
  , val0
  , val1
  , val2
  )
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (Cons, Nil, RProxy(RProxy))


_suite :: TestSuite
_suite =
  suite "builderDecodeJsonWith'" do
--     test "#0" do
--       let
--         result :: Either String (Builder {} Type_0)
--         result =
--           builderDecodeJsonWith'
--             (RProxy :: RProxy ())
--             (RProxy :: RProxy (TypeRep_0 Maybe ()))
--             (RProxy :: RProxy (TypeRep_0 Maybe ()))
--             { a0: \json -> Right $ 1000
--             , a1: \json -> Right $ 1001
--             }
--             (encodeJson val0)
--       Assert.assert "error msg" true
--     test "#1" do
--       let
--         result :: Either String (Builder {} {})
--         result =
--           builderDecodeJsonWith'
--             (RProxy :: RProxy ())
--             (RProxy :: RProxy ())
--             (RProxy :: RProxy ())
--             {}
--             (encodeJson {})
--       Assert.assert "error msg" true
--     test "#2" do
--       let
--         result :: Either String (Builder Type_0 Type_0)
--         result =
--           builderDecodeJsonWith'
--             (RProxy :: RProxy (TypeRep_0 Maybe ()))
--             (RProxy :: RProxy ())
--             (RProxy :: RProxy (TypeRep_0 Maybe ()))
--             {}
--             (encodeJson val0)
--       Assert.assert "error msg" true
--     test "#3" do
--       let
--         result :: Either String (Builder {} { a0 :: Int })
--         result =
--           builderDecodeJsonWith'
--             (RProxy :: RProxy ())
--             (RProxy :: RProxy ( a0 :: Int ))
--             (RProxy :: RProxy ( a0 :: Int ))
--             { a0: \json -> Just 0 }
--             (encodeJson {})
--       Assert.assert "error msg" true

    test "#4" do
      let
        result :: Either String (Builder { b1 :: Int } { b0 :: Int, b1 :: Int })
        result =
          builderDecodeJsonWith'
            (RLProxy :: RLProxy (Cons "b1" Int Nil))
            (RLProxy :: RLProxy (Cons "b0" Int Nil))
            (RLProxy :: RLProxy (Cons "b0" Int (Cons "b1" Int Nil)))
            --(RLProxy :: RLProxy (Cons "b1" Int (Cons "b0" Int Nil)))

--             (RProxy :: RProxy ( b0 :: Int ))
--             (RProxy :: RProxy ( b1 :: Int ))
--             (RProxy :: RProxy ( b0 :: Int, b1 :: Int ))
            { b0: \json -> Just 100 }
            (encodeJson { b0: 0 })
      Assert.assert "error msg" true
