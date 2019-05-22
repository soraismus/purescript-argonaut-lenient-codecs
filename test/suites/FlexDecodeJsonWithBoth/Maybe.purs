module Test.Suites.FlexDecodeJsonWithBoth.Maybe
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Decode.Flex (flexDecodeJsonWithBoth)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Just, Nothing))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, check, fails, withErrorMsg)

suitex :: TestSuite
suitex =
  suite "Maybe" do
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
              flexDecodeJsonWithBoth
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                { a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ check result withErrorMsg
            (_ == { a0: 100
                  , a1: 101
                  , a2: Nothing
                  , a3: Nothing
                  , a4: Nothing
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
              flexDecodeJsonWithBoth
                {}
                { a2: \json -> Right $ Just 102
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
        test "#2" do
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
              flexDecodeJsonWithBoth
                {}
                { a2: \json -> Right $ Just 102 }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
        test "#3" do
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
              flexDecodeJsonWithBoth
                { a4: \json -> Right $ Just false }
                { a2: \json -> Right $ Just 102 }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
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
                flexDecodeJsonWithBoth
                  { a0: \json -> Right 100
                  , a1: \json -> Right 101
                  }
                  { a2: \json -> Right $ Just 102
                  , a3: \json -> Right $ Just "bye"
                  , a4: \json -> Right $ Just false
                  }
                  (encodeJson { a0: 0, a1: 1, a2: Just 2 })
            assert $ check result withErrorMsg
              (_ == { a0: 100
                    , a1: 101
                    , a2: Just 102
                    , a3: Nothing
                    , a4: Nothing
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
                flexDecodeJsonWithBoth
                  {}
                  { a2: \json -> Right $ Just 102
                  , a4: \json -> Right $ Just false
                  }
                  (encodeJson { a0: 0, a1: 1, a2: Just 2 })
            assert $ fails result
          test "#2" do
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
                flexDecodeJsonWithBoth
                  {}
                  { a2: \json -> Right $ Just 102
                  , a3: \json -> Right $ Just "bye"
                  , a4: \json -> Right $ Just false
                  }
                  (encodeJson { a0: 0, a1: 1, a2: Just 2 })
            assert $ check result withErrorMsg
              (_ == { a0: { a0: 0, a1: 1, a2: Just 2 }.a0
                    , a1: { a0: 0, a1: 1, a2: Just 2 }.a1
                    , a2: Just 102
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
                flexDecodeJsonWithBoth
                  { a0: \json -> Right 100
                  , a1: \json -> Right 101
                  }
                  { a2: \json -> Right $ Just 102
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
              (_ == { a0: 100
                    , a1: 101
                    , a2: Just 102
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
                flexDecodeJsonWithBoth
                  { a0: \json -> Right 100
                  , a1: \json -> Right 101
                  , a3: \json -> Right $ Just "bye"
                  }
                  { a2: \json -> Right $ Just 102
                  , a4: \json -> Right $ Just false
                  }
                  (encodeJson { a0: 0
                              , a1: 1
                              , a2: Just 2
                              , a3: Just "hello"
                              , a4: Just true
                              })
            assert $ check result withErrorMsg
              (_ == { a0: 100
                    , a1: 101
                    , a2: Just 102
                    , a3: Just "bye"
                    , a4: Just false
                    })
          test "#2" do
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
                flexDecodeJsonWithBoth
                  {}
                  { a2: \json -> Right $ Just 102
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
                    , a2: Just 102
                    , a3: Just "hello"
                    , a4: Just false
                    })
