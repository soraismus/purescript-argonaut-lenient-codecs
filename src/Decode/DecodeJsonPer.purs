module Data.Argonaut.Decode.Spec where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Cases (class Cases)
import Data.Argonaut.Utils (getMissingFieldErrorMessage, reportJson)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Record (get, insert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Equality (class TypeEquals, to)
import Type.Row
  ( class Cons
  , class Lacks
  , class RowToList
  , Cons
  , Nil
  , kind RowList
  )
import Unsafe.Coerce (unsafeCoerce)

class DecodeJsonPer_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  | l1 -> r1 l0 r0 where
  decodeJsonPer_
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> f (Record r0)

instance decodeJsonPer_Nil
  :: Status f
  => DecodeJsonPer_ f Nil () Nil () where
  decodeJsonPer_ _ _ _ _ = report {}

instance decodeJsonPer_Cons
  :: ( Bind f
     , Cases f dl r
     , Cases f dl' r'
     , Cons s v r' r
     , Cons s dv dr' dr
     , DecodeJsonPer_ f dl' dr' l' r'
     , IsSymbol s
     , Lacks s r'
     , Lacks s dr'
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , Status f
     , TypeEquals dv (Json -> f v)
     )
  => DecodeJsonPer_ f (Cons s dv dl') dr (Cons s v l') r
  where
  decodeJsonPer_ _ _ decoderRecord object = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> f v
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      decodeJsonPer_
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl')
        decoderRecord'
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ insert sProxy val rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

class
  ( Cases f l1 r0
  , RowToList r1 l1
  ) <=
  DecodeJsonPer
    (f :: Type -> Type)
    (l1 :: RowList)
    (r1 :: # Type)
    (r0 :: # Type)
    | r0 -> r1 l1 where
    decodeJsonPer :: Record r1 -> Json -> f (Record r0)

instance decodeJsonPerDecodeJsonPer_
  :: ( Cases f l1 r0
     , DecodeJsonPer_ f l1 r1 l0 r0
     , RowToList r0 l0
     , RowToList r1 l1
     , Status f
     )
  => DecodeJsonPer f l1 r1 r0
  where
  decodeJsonPer decoderRecord =
    reportJson $
      decodeJsonPer_
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
        decoderRecord
