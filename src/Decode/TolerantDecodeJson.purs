module Data.Argonaut.Decode.Tolerant
  ( class TolerantDecodeJson
  , class TolerantGDecodeJson
  , tolerantDecodeJson
  , tolerantGDecodeJson
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

class TolerantGDecodeJson f (r :: # Type) (l :: RowList) | l -> r where
  tolerantGDecodeJson :: Object Json -> RLProxy l -> f (Record r)

instance tolerantGDecodeJsonNil
  :: Status f
  => TolerantGDecodeJson f () Nil where
  tolerantGDecodeJson _ _ = report {}

instance tolerantGDecodeJsonCons_Plus
  :: ( Bind g
     , Cons s (f v) rTail r
     , DecodeJson (f v)
     , TolerantGDecodeJson g rTail tail
     , IsSymbol s
     , Lacks s rTail
     , Plus f
     , Status g
     )
  => TolerantGDecodeJson g r (Cons s (f v) tail) where
  tolerantGDecodeJson object _ = do
    let sProxy = SProxy :: SProxy s
    let fieldName = reflectSymbol sProxy
    rest <- tolerantGDecodeJson object (RLProxy :: RLProxy tail)
    case lookup fieldName object of
      Just jsonVal ->
        case decodeJson jsonVal of
          Left errorStr -> reportError errorStr
          Right val     -> report $ insert sProxy val rest
      Nothing ->
        report $ insert sProxy empty rest

else instance tolerantGDecodeJsonCons_nonPlus
  :: ( Bind g
     , Cons s v rTail r
     , DecodeJson v
     , TolerantGDecodeJson g rTail tail
     , IsSymbol s
     , Lacks s rTail
     , Status g
     )
  => TolerantGDecodeJson g r (Cons s v tail) where
  tolerantGDecodeJson object _ = do
    let sProxy = SProxy :: SProxy s
    let fieldName = reflectSymbol sProxy
    rest <- tolerantGDecodeJson object (RLProxy :: RLProxy tail)
    case lookup fieldName object of
      Just jsonVal ->
        case decodeJson jsonVal of
          Left errorStr -> reportError errorStr
          Right val     -> report $ insert sProxy val rest
      Nothing ->
        reportError $ "JSON was missing expected field: " <> fieldName

class TolerantDecodeJson f a where
  tolerantDecodeJson :: Json -> f a

instance tolerantDecodeJsonRecord
  :: ( TolerantGDecodeJson f r l
     , RowToList r l
     , Status f
     )
  => TolerantDecodeJson f (Record r) where
  tolerantDecodeJson = reportJson $ flip tolerantGDecodeJson (RLProxy :: RLProxy l)
