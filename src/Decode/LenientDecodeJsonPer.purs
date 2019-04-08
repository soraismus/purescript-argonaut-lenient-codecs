module Data.Argonaut.Decode.Lenient.Per where

import Prelude (class Bind, bind, ($))

import Control.Plus (class Plus, empty)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Cases
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

class LenientDecodeJsonPer_
  (f :: Type -> Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  | l1 -> r1 l0 r0 where
  lenientDecodeJsonPer_
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> f (Record r0)

instance lenientDecodeJsonPer_Nil
  :: Status f
  => LenientDecodeJsonPer_ f Nil () Nil () where
  lenientDecodeJsonPer_ _ _ _ _ = report {}

instance lenientDecodeJsonPer_Cons_Plus
  :: ( Bind g
     , Cases g dl r
     , Cases g dl' r'
     , Cons s (f v) r' r
     , Cons s dv dr' dr
     , LenientDecodeJsonPer_ g dl' dr' l' r'
     , IsSymbol s
     , Lacks s r'
     , Lacks s dr'
     , Plus f
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , Status g
     , TypeEquals dv (Json -> g (f v))
     )
  => LenientDecodeJsonPer_ g (Cons s dv dl') dr (Cons s (f v) l') r
  where
  lenientDecodeJsonPer_ _ _ decoderRecord object = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> g (f v)
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      lenientDecodeJsonPer_
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

else instance lenientDecodeJsonPer_Cons_nonPlus
  :: ( Bind g
     , Cases g dl r
     , Cases g dl' r'
     , Cons s v r' r
     , Cons s dv dr' dr
     , LenientDecodeJsonPer_ g dl' dr' l' r'
     , IsSymbol s
     , Lacks s r'
     , Lacks s dr'
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , Status g
     , TypeEquals dv (Json -> g v)
     )
  => LenientDecodeJsonPer_ g (Cons s dv dl') dr (Cons s v l') r
  where
  lenientDecodeJsonPer_ _ _ decoderRecord object = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> g v
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy` to
      -- induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      lenientDecodeJsonPer_
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
  LenientDecodeJsonPer
    (f :: Type -> Type)
    (l1 :: RowList)
    (r1 :: # Type)
    (r0 :: # Type)
    | r0 -> r1 l1 where
    lenientDecodeJsonPer :: Record r1 -> Json -> f (Record r0)

instance lenientDecodeJsonPerLenientDecodeJsonPer_
  :: ( Cases f l1 r0
     , LenientDecodeJsonPer_ f l1 r1 l0 r0
     , RowToList r0 l0
     , RowToList r1 l1
     , Status f
     )
  => LenientDecodeJsonPer f l1 r1 r0
  where
  lenientDecodeJsonPer decoderRecord =
    reportJson $
      lenientDecodeJsonPer_
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
        decoderRecord
