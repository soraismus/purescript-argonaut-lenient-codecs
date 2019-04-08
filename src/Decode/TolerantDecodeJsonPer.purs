module Data.Argonaut.Decode.Tolerant.Per where

import Prelude (class Bind, bind, ($))

import Control.Plus (class Plus, empty)
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

class TolerantDecodeJsonPer_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  | l1 -> r1 l0 r0 where
  tolerantDecodeJsonPer_
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> f (Record r0)

instance tolerantDecodeJsonPer_Nil
  :: Status f
  => TolerantDecodeJsonPer_ f Nil () Nil () where
  tolerantDecodeJsonPer_ _ _ _ _ = report {}

instance tolerantDecodeJsonPer_Cons_Plus
  :: ( Bind f
     , Cases dl r
     , Cases dl' r'
     , Cons s (g v) r' r
     , Cons s dv dr' dr
     , TolerantDecodeJsonPer_ f dl' dr' l' r'
     , IsSymbol s
     , Lacks s r'
     , Lacks s dr'
     , Plus g
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , Status f
     , TypeEquals dv (Json -> f (g v))
     )
  => TolerantDecodeJsonPer_ f (Cons s dv dl') dr (Cons s (g v) l') r
  where
  tolerantDecodeJsonPer_ _ _ decoderRecord object = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> f (g v)
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      tolerantDecodeJsonPer_
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl')
        decoderRecord'
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        report $ insert sProxy val rest
      Nothing ->
        report $ insert sProxy empty rest

else instance tolerantDecodeJsonPer_Cons_nonPlus
  :: ( Bind f
     , Cases dl r
     , Cases dl' r'
     , Cons s v r' r
     , Cons s dv dr' dr
     , TolerantDecodeJsonPer_ f dl' dr' l' r'
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
  => TolerantDecodeJsonPer_ f (Cons s dv dl') dr (Cons s v l') r
  where
  tolerantDecodeJsonPer_ _ _ decoderRecord object = do
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
      tolerantDecodeJsonPer_
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
  ( Cases l1 r0
  , RowToList r1 l1
  ) <=
  TolerantDecodeJsonPer
    (f :: Type -> Type)
    (l1 :: RowList)
    (r1 :: # Type)
    (r0 :: # Type)
    | r0 -> r1 l1 where
    tolerantDecodeJsonPer :: Record r1 -> Json -> f (Record r0)

instance tolerantDecodeJsonPerTolerantDecodeJsonPer_
  :: ( Cases l1 r0
     , TolerantDecodeJsonPer_ f l1 r1 l0 r0
     , RowToList r0 l0
     , RowToList r1 l1
     , Status f
     )
  => TolerantDecodeJsonPer f l1 r1 r0
  where
  tolerantDecodeJsonPer decoderRecord =
    reportJson $
      tolerantDecodeJsonPer_
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
        decoderRecord
