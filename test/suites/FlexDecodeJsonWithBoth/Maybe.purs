module Test.Suites.FlexDecodeJsonWithBoth.Maybe
  ( _suite
  ) where

import Prelude

import Data.Argonaut.Decode.Flex (flexDecodeJsonWithBoth)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Just, Nothing))
import Test.Unit (TestSuite, suite, test)
import Test.Utils
  ( Type_2
  , assert
  , check
  , doesntMeet
  , fails
  , val0
  , val1
  , val2
  )

_suite :: TestSuite
_suite =
  suite "Maybe" do
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
