module Data.Argonaut.Decode.Mh where

import Prelude (class Bind, bind, flip, pure, ($), (<>))

import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Cases
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  )
import Data.Argonaut.Decode.Standard
  ( class DecodeJsonWith_
  , __decodeJsonWith
  )
import Data.Argonaut.Utils (reportJson, reportObject)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Record (get, insert, merge, union)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Equality (class TypeEquals, to)
import Type.Row
  ( class Cons
  , class Lacks
  , class Nub
  , class RowToList
  , class Union
  , Cons
  , Nil
  , RProxy
  , kind RowList
  )
import Unsafe.Coerce (unsafeCoerce)

class MhGDecodeJson f (r :: # Type) (l :: RowList) | l -> r where
  mhGDecodeJson :: Object Json -> RLProxy l -> f (Record r)

instance mhGDecodeJsonNil
  :: Status f
  => MhGDecodeJson f () Nil where
  mhGDecodeJson _ _ = report {}

instance mhGDecodeJsonCons_Alternative
  :: ( Alternative f
     , Bind g
     , Cons s (f v) rTail r
     , DecodeJson v
     , MhGDecodeJson g rTail tail
     , IsSymbol s
     , Lacks s rTail
     , Status g
     )
  => MhGDecodeJson g r (Cons s (f v) tail) where
  mhGDecodeJson object _ = do
    let sProxy = SProxy :: SProxy s
    let fieldName = reflectSymbol sProxy
    rest <- mhGDecodeJson object (RLProxy :: RLProxy tail)
    case lookup fieldName object of
      Just jsonVal ->
        case decodeJson jsonVal of
          Left errorStr -> reportError errorStr
          Right val     -> report $ insert sProxy (pure val) rest
      Nothing ->
        report $ insert sProxy empty rest
else instance mhGDecodeJsonCons_nonAlternative
  :: ( Alternative f
     , Bind g
     , Cons s v rTail r
     , DecodeJson v
     , MhGDecodeJson g rTail tail
     , IsSymbol s
     , Lacks s rTail
     , Status g
     )
  => MhGDecodeJson g r (Cons s v tail) where
  mhGDecodeJson object _ = do
    let sProxy = SProxy :: SProxy s
    let fieldName = reflectSymbol sProxy
    rest <- mhGDecodeJson object (RLProxy :: RLProxy tail)
    case lookup fieldName object of
      Just jsonVal ->
        case decodeJson jsonVal of
          Left errorStr -> reportError errorStr
          Right val     -> report $ insert sProxy val rest
      Nothing ->
        reportError $ "JSON was missing expected field: " <> fieldName
