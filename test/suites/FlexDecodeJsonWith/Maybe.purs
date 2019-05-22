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
import Test.Utils (assert, check, withErrorMsg)

suitex :: TestSuite
suitex =
  suite "Maybe" do
    suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int }" do
      suite "{ a0: 0, a1: 1 }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002 }
                (encodeJson { a0: 0, a1: 1 })
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: Nothing })
      suite "{ a0: 0, a1: 1, a2: Just 2 }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002 }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: Just 1002 })
      suite "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002 }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: Just 1002 })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              flexDecodeJsonWith
                { a2: decodeJson' }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
              where
              decodeJson' :: Json -> Either String (Maybe Int)
              decodeJson' = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: Just 2 })
    suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int, a3 :: Maybe String, a4 :: Maybe Boolean }" do
      suite "{ a0: 0, a1: 1 }" do
        test "#0" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Maybe Int
                  , a3 :: Maybe String
                  , a4 :: Maybe Boolean
                  }
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ check result withErrorMsg
            (_ == { a0: 0
                  , a1: 1
                  , a2: Nothing
                  , a3: Nothing
                  , a4: Nothing
                  })
      suite "{ a0: 0, a1: 1, a2: Just 2 }" do
        test "#0" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Maybe Int
                  , a3 :: Maybe String
                  , a4 :: Maybe Boolean
                  }
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ check result withErrorMsg
            (_ == { a0: 0
                  , a1: 1
                  , a2: Just 1002
                  , a3: Nothing
                  , a4: Nothing
                  })
      suite "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
        test "#0" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Maybe Int
                  , a3 :: Maybe String
                  , a4 :: Maybe Boolean
                  }
            result =
              flexDecodeJsonWith
                { a2: \json -> Right $ Just 1002
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ check result withErrorMsg
            (_ == { a0: 0
                  , a1: 1
                  , a2: Just 1002
                  , a3: Just "bye"
                  , a4: Just false
                  })
        test "#1" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Maybe Int
                  , a3 :: Maybe String
                  , a4 :: Maybe Boolean
                  }
            result =
              flexDecodeJsonWith
                { a2: decodeJson2
                , a3: decodeJson3
                , a4: decodeJson4
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
                where
                decodeJson2 :: Json -> Either String (Maybe Int)
                decodeJson2 = decodeJson
                decodeJson3 :: Json -> Either String (Maybe String)
                decodeJson3 = decodeJson
                decodeJson4 :: Json -> Either String (Maybe Boolean)
                decodeJson4 = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: Just 2, a3: Just "hello", a4: Just true })
