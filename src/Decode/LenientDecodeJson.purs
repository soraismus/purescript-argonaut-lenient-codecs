module Data.Argonaut.Decode.Lenient
  ( class LenientDecodeJson
  , class LenientGDecodeJson
  , lenientDecodeJson
  , lenientGDecodeJson
  ) where

import Prelude (class Bind, bind, flip, ($), (<>))

import Control.Plus (class Plus, empty)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Utils (reportJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Record (insert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row
  ( class Cons
  , class Lacks
  , class RowToList
  , Cons
  , Nil
  , kind RowList
  )

class LenientGDecodeJson f (r :: # Type) (l :: RowList) | l -> r where
  lenientGDecodeJson :: Object Json -> RLProxy l -> f (Record r)

instance lenientGDecodeJsonNil
  :: Status f
  => LenientGDecodeJson f () Nil where
  lenientGDecodeJson _ _ = report {}

instance lenientGDecodeJsonCons_Plus
  :: ( Bind g
     , Cons s (f v) rTail r
     , DecodeJson (f v)
     , LenientGDecodeJson g rTail tail
     , IsSymbol s
     , Lacks s rTail
     , Plus f
     , Status g
     )
  => LenientGDecodeJson g r (Cons s (f v) tail) where
  lenientGDecodeJson object _ = do
    let sProxy = SProxy :: SProxy s
    let fieldName = reflectSymbol sProxy
    rest <- lenientGDecodeJson object (RLProxy :: RLProxy tail)
    case lookup fieldName object of
      Just jsonVal ->
        case decodeJson jsonVal of
          Left errorStr -> reportError errorStr
          Right val     -> report $ insert sProxy val rest
      Nothing ->
        report $ insert sProxy empty rest

else instance lenientGDecodeJsonCons_nonPlus
  :: ( Bind g
     , Cons s v rTail r
     , DecodeJson v
     , LenientGDecodeJson g rTail tail
     , IsSymbol s
     , Lacks s rTail
     , Status g
     )
  => LenientGDecodeJson g r (Cons s v tail) where
  lenientGDecodeJson object _ = do
    let sProxy = SProxy :: SProxy s
    let fieldName = reflectSymbol sProxy
    rest <- lenientGDecodeJson object (RLProxy :: RLProxy tail)
    case lookup fieldName object of
      Just jsonVal ->
        case decodeJson jsonVal of
          Left errorStr -> reportError errorStr
          Right val     -> report $ insert sProxy val rest
      Nothing ->
        reportError $ "JSON was missing expected field: " <> fieldName

class LenientDecodeJson f a where
  lenientDecodeJson :: Json -> f a

instance lenientDecodeJsonRecord
  :: ( LenientGDecodeJson f r l
     , RowToList r l
     , Status f
     )
  => LenientDecodeJson f (Record r) where
  lenientDecodeJson = reportJson $ flip lenientGDecodeJson (RLProxy :: RLProxy l)
