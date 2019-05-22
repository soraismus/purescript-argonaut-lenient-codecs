module Test.Suites.DecodeJsonPer.Maybe
  ( suitex
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
  ( assert
  , capriciousFailure
  , check
  , check'
  , fails
  , notVal0
  , notVal1
  , withErrorMsg
  )

suitex :: TestSuite
suitex =
  suite "Maybe" do
    suite "{ a0 :: Int, a1 :: Int }" do
      suite "{ a0: 0, a1: 1 }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101 })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson { a0: 0, a1: 1 })
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check' result (_ == { a0: 0, a1: 1 }) otherwise notVal0
      suite "{ a0: 0, a1: 1, a2: Just 2 }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101 })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ fails result
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1 })
      suite "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101 })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ fails result
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1 })
    suite "{ a0 :: Int, a1 :: Int, a2 :: Maybe Int }" do
      suite "{ a0: 0, a1: 1 }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                }
                (encodeJson { a0: 0, a1: 1 })
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
              decodeJsonMaybeInt = decodeJson
          assert $ fails result
        test "#4" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
      suite "{ a0: 0, a1: 1, a2: Just 2 }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101, a2: Just 102 })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ fails result
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
              decodeJsonMaybeInt = decodeJson
          assert $ check' result (_ == { a0: 0, a1: 1, a2: Just 2 }) otherwise notVal1
        test "#4" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101, a2: Nothing })
      suite "{ a0: 0, a1: 1, a2: Just 2, a3: Just \"hello\", a4: Just true }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101, a2: Just 102 })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ fails result
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
              decodeJsonMaybeInt = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: Just 2 })
        test "#4" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Maybe Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101, a2: Nothing })
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
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
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
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
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
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
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
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                , a3: decodeJsonMaybeString
                , a4: decodeJsonMaybeBoolean
                }
                (encodeJson { a0: 0, a1: 1 })
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
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
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
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ fails result
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
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
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
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
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
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                , a3: decodeJsonMaybeString
                , a4: decodeJsonMaybeBoolean
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
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
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0, a1: 1, a2: Just 2 })
          assert $ fails result
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
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
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
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ Just 102
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
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
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ Just "bye"
                , a4: \json -> Right $ Just false
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
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
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonMaybeInt
                , a3: decodeJsonMaybeString
                , a4: decodeJsonMaybeBoolean
                }
                (encodeJson { a0: 0
                            , a1: 1
                            , a2: Just 2
                            , a3: Just "hello"
                            , a4: Just true
                            })
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonMaybeInt :: Json -> Either String (Maybe Int)
              decodeJsonMaybeInt = decodeJson
              decodeJsonMaybeString :: Json -> Either String (Maybe String)
              decodeJsonMaybeString = decodeJson
              decodeJsonMaybeBoolean :: Json -> Either String (Maybe Boolean)
              decodeJsonMaybeBoolean = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0
                  , a1: 1
                  , a2: Just 2
                  , a3: Just "hello"
                  , a4: Just true
                  })
        test "#4" do
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
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ Nothing
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
                  , a2: Nothing
                  , a3: Just "bye"
                  , a4: Just false
                  })
