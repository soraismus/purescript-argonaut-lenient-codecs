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
  ( assert
  , capriciousFailure
  , check
  , check'
  , fails
  , notVal3
  , notVal4
  , notVal5
  , withErrorMsg
  )

suitex :: TestSuite
suitex =
  suite "Array" do
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
          assert $ check' result (_ == { a0: 0, a1: 1 }) otherwise notVal3
      suite "{ a0: 0, a1: 1, a2: [2] }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
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
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1 })
      suite "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
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
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Left capriciousFailure
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ fails result
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int }
            result =
              decodeJsonPer
                { a0: decodeJson'
                , a1: decodeJson'
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
              where
              decodeJson' :: Json -> Either String Int
              decodeJson' = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1 })
    suite "{ a0 :: Int, a1 :: Int, a2 :: Array Int }" do
      suite "{ a0: 0, a1: 1 }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
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
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                }
                (encodeJson { a0: 0, a1: 1 })
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonArrayInt :: Json -> Either String (Array Int)
              decodeJsonArrayInt = decodeJson
          assert $ fails result
        test "#4" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
      suite "{ a0: 0, a1: 1, a2: [2] }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101, a2: [102] })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonArrayInt :: Json -> Either String (Array Int)
              decodeJsonArrayInt = decodeJson
          assert $ check' result (_ == { a0: 0, a1: 1, a2: [2] }) otherwise notVal4
        test "#4" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101, a2: [] })
      suite "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
        test "#0" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101, a2: [102] })
        test "#1" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ fails result
        test "#2" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ fails result
        test "#3" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonArrayInt :: Json -> Either String (Array Int)
              decodeJsonArrayInt = decodeJson
          assert $ check result withErrorMsg
            (_ == { a0: 0, a1: 1, a2: [2] })
        test "#4" do
          let
            result :: Either String { a0 :: Int, a1 :: Int, a2 :: Array Int }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 100, a1: 101, a2: [] })
    suite "{ a0 :: Int, a1 :: Int, a2 :: Array Int, a3 :: Array String, a4 :: Array Boolean }" do
      suite "{ a0: 0, a1: 1 }" do
        test "#0" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
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
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
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
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
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
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                , a3: decodeJsonArrayString
                , a4: decodeJsonArrayBoolean
                }
                (encodeJson { a0: 0, a1: 1 })
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
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1 })
          assert $ fails result
      suite "{ a0: 0, a1: 1, a2: [2] }" do
        test "#0" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
        test "#1" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
        test "#2" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
        test "#3" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                , a3: decodeJsonArrayString
                , a4: decodeJsonArrayBoolean
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
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
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2] })
          assert $ fails result
      suite "{ a0: 0, a1: 1, a2: [2], a3: [\"hello\"], a4: [true] }" do
        test "#0" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 100
                  , a1: 101
                  , a2: [102]
                  , a3: ["bye"]
                  , a4: [false]
                  })
        test "#1" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Left capriciousFailure
                , a1: \json -> Right 101
                , a2: \json -> Right $ [102]
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ fails result
        test "#2" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Left capriciousFailure
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ fails result
        test "#3" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: decodeJsonInt
                , a1: decodeJsonInt
                , a2: decodeJsonArrayInt
                , a3: decodeJsonArrayString
                , a4: decodeJsonArrayBoolean
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
              where
              decodeJsonInt :: Json -> Either String Int
              decodeJsonInt = decodeJson
              decodeJsonArrayInt :: Json -> Either String (Array Int)
              decodeJsonArrayInt = decodeJson
              decodeJsonArrayString :: Json -> Either String (Array String)
              decodeJsonArrayString = decodeJson
              decodeJsonArrayBoolean :: Json -> Either String (Array Boolean)
              decodeJsonArrayBoolean = decodeJson
          assert $
            check'
              result (_ == { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
              otherwise notVal5
        test "#4" do
          let
            result
              :: Either
                  String
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Array Int
                  , a3 :: Array String
                  , a4 :: Array Boolean
                  }
            result =
              decodeJsonPer
                { a0: \json -> Right 100
                , a1: \json -> Right 101
                , a2: \json -> Right $ []
                , a3: \json -> Right $ ["bye"]
                , a4: \json -> Right $ [false]
                }
                (encodeJson { a0: 0, a1: 1, a2: [2], a3: ["hello"], a4: [true] })
          assert $ check result withErrorMsg
            (_ == { a0: 100
                  , a1: 101
                  , a2: []
                  , a3: ["bye"]
                  , a4: [false]
                  })
