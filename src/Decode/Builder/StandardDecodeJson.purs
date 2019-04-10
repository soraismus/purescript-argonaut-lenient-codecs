module Data.Argonaut.Decode.Standard.Builder where

import Prelude (class Bind, bind, identity, ($), (<<<))

-- import Control.Alt (class Alt)
-- import Control.Alternative (class Alternative, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Cases (class Cases)
-- import Data.Argonaut.Decode.Class
--   ( class DecodeJson
--   , class GDecodeJson
--   , decodeJson
--   , gDecodeJson
--   )
import Data.Argonaut.Utils (getMissingFieldErrorMessage)
-- import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
-- import Record (delete, get, insert, merge, union)
import Record (get)
import Record.Builder (Builder)
-- import Record.Builder (build, delete, insert, merge, union) as Builder
import Record.Builder (insert) as Builder
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Equality (class TypeEquals, to)
-- import Type.Proxy (Proxy(Proxy))
import Type.Row
  ( kind RowList
  , Cons
  , Nil
--   , RProxy(RProxy)
  , RProxy
  , class Cons
  , class Lacks
  , class ListToRow
--   , class Nub
  , class RowListRemove
  , class RowToList
--   , class Union
  )
-- import Type.Row as Row
import Unsafe.Coerce (unsafeCoerce)

class BuilderDecodeJsonWith_
  (f :: Type -> Type)
  (sl :: RowList)
  (sr :: # Type)
  (l :: RowList)
  (r :: # Type)
  (dl :: RowList)
  (dr :: # Type)
  (tl :: RowList)
  (tr :: # Type)
  | sl -> sr, tl -> tr, dl -> dr l r, dl sl -> tl, sl tl -> dl, dl tl -> sl
  -- | dl -> dr l r, sl l -> tl, sl -> sr where
  where
  __builderDecodeJsonWith
    :: RLProxy sl
    -> RLProxy l
    -> RLProxy dl
    -> RLProxy tl
    -> Record dr
    -> Object Json
    -> f (Builder (Record sr) (Record tr))

-- instance __builderDecodeJsonWith_NilSource
--   :: Status f
--   => BuilderDecodeJsonWith_ f    Nil ()   l r     dl dr     l r
--   where
--   __builderDecodeJsonWith _ _ _ _    _ _ = report identity

instance __builderDecodeJsonWith_NilChange
  :: Status f
  => BuilderDecodeJsonWith_ f    l r   Nil ()     Nil ()     l r
  where
  __builderDecodeJsonWith _ _ _ _    _ _ = report identity

else instance __builderDecodeJsonWith_One
  :: ( Bind f
     , BuilderDecodeJsonWith_ f sl sr Nil () Nil () tl2' tr2' -- true if sr == tr2'
     , Cases dl r
     , Cons ds v sr tr

     , Cons ds v () r
     , Cons ds dv () dr

     , Cons ts tv tr' tr
     , Lacks ds sr
     , Lacks ts tr'
     , IsSymbol ds
     , IsSymbol ts
     , ListToRow tl2' tr2'
     , RowListRemove ds tl tl2'
     , RowToList r l
     , RowToList dr dl
     , RowToList sr sl
     , RowToList tr tl
     , RowToList tr' tl'
     , Status f
     , TypeEquals dv (Json -> f v)

     , TypeEquals (RProxy tr2') (RProxy sr)
     )
  => BuilderDecodeJsonWith_
       f
       sl
       sr
       (Cons ds v Nil)
       r
       (Cons ds dv Nil)
       dr
       (Cons ts tv tl')
       tr
  where
  __builderDecodeJsonWith _ _ _ _ decoderRecord object = do
    let
      sProxy :: SProxy ds
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> f v
      decoder = to $ get sProxy decoderRecord

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        let
          builder :: Builder (Record sr) (Record tr)
          builder = Builder.insert sProxy val
        report builder
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

else instance __builderDecodeJsonWith_Many
  :: ( Bind f
     , BuilderDecodeJsonWith_ f sl sr l' r' dl' dr' tl2' tr2'
     , Cases dl r
     , Cases dl' r'
     , Cons ds v sr tr
     , Cons ds v r' r
     , Cons ds dv dr' dr
     , Cons ts tv tr' tr

     , Cons ds v tr2' tr2
     , Lacks ds tr2'
     , RowToList tr2 tl2
     , RowToList tr2' tl2'
     , RowListRemove ds tl tl2'
     , TypeEquals (RProxy tr2) (RProxy tr)

     , Lacks ds sr
     , Lacks ds r'
     , Lacks ds dr'
     , Lacks ts tr'
     , IsSymbol ds
     , IsSymbol ts
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , RowToList sr sl
     , RowToList tr tl
     , RowToList tr' tl'
     , Status f
     , TypeEquals dv (Json -> f v)
     )
  => BuilderDecodeJsonWith_
       f
       sl
       sr
       (Cons ds v l')
       r
       (Cons ds dv dl')
       dr
       (Cons ts tv tl')
       tr
  where
  __builderDecodeJsonWith _ _ _ _ decoderRecord object = do
    let
      sProxy :: SProxy ds
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

    (rest :: Builder (Record sr) (Record tr2')) <-
      __builderDecodeJsonWith
        (RLProxy :: RLProxy sl)
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl')
        (RLProxy :: RLProxy tl2')
        decoderRecord'
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal

        let
          builder' :: Builder (Record sr) (Record tr2)
          builder' = Builder.insert sProxy val <<< rest

          -- Calling `to` to convert from `tr2` to `tr` isn't possible
          -- since Builder's constructors are not publicly accessible.
          -- Thus, `unsafeCoerce` is used instead.
          builder :: Builder (Record sr) (Record tr)
          builder = unsafeCoerce builder'

        report builder
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

-- class BuilderDecodeJsonWith_
--   (f :: Type -> Type)
--   (sl :: RowList)
--   (sr :: # Type)
--
--   (l :: RowList)
--   (r :: # Type)
--
--   (dl :: RowList)
--   (dr :: # Type)
--
--   (tl :: RowList)
--   (tr :: # Type)
--   | sl -> sr, tl -> tr, dl -> dr l r, dl sl -> tl, sl tl -> dl, dl tl -> sl
--   -- | dl -> dr l r, sl l -> tl, sl -> sr where
--   where
--   __builderDecodeJsonWith
--     :: RLProxy sl
--     -> RLProxy l
--     -> RLProxy dl
--     -> RLProxy tl
--     -> Record dr
--     -> Object Json
--     -> f (Builder (Record sr) (Record tr))

builderDecodeJsonWith'
  :: forall f sl sr l r dl dr tl tr
   . BuilderDecodeJsonWith_ f sl sr l r dl dr tl tr
  => Status f
  -- => Union r sr tr

  => RLProxy sl
  -> RLProxy l
  -> RLProxy tl

  -> Record dr
  -> Json
  -> f (Builder (Record sr) (Record tr))
builderDecodeJsonWith' _ _ _ decoderRecord json =
  case toObject json of
    Just object -> go object
    Nothing -> reportError notObjectErrorMessage
  where
  go :: Object Json -> f (Builder (Record sr) (Record tr))
  go object =
    __builderDecodeJsonWith
      (RLProxy :: RLProxy sl)
      (RLProxy :: RLProxy l)
      (RLProxy :: RLProxy dl)
      (RLProxy :: RLProxy tl)
      decoderRecord
      object

notObjectErrorMessage :: String
notObjectErrorMessage = "Could not convert JSON to object"
-- class
--   ( Cases l1 r0
--   , RowToList r1 l1
--   ) <=
--   BuilderDecodeJsonWith
--     (f :: Type -> Type)
--     (l1 :: RowList)
--     (r1 :: # Type)
--     (r0 :: # Type)
--     | r0 -> r1 l1 where
--     builderDecodeJsonWith'
--       :: Record r1
--       -> Json
--       -> f (Builder {} (Record r0))

-- instance builderDecodeJsonWithDecodeJsonWith_
--   :: ( BuilderDecodeJsonWith_ f l1 r1 l0 r0
--      , Cases l1 r0
--      , RowToList r0 l0
--      , RowToList r1 l1
--      , Status f
--      )
--   => BuilderDecodeJsonWith f l1 r1 r0
--   where
--   builderDecodeJsonWith' decoderRecord =
--     reportBuilderJson $
--       __builderDecodeJsonWith
--         (RLProxy :: RLProxy l0)
--         (RLProxy :: RLProxy l1)
--         decoderRecord

-- builderDecodeJsonWith
--   :: forall dr dl f l0 l1 l2 r0 r1 r2
--    . Bind f
--   => BuilderDecodeJsonWith_ f dl dr l0 r0
--   => GDecodeJson r1 l1
--   => Nub r2 r2
--   => RowToList r1 l1
--   => RowToList r2 l2
--   => RowToList dr dl
--   => Status f
--   => Union r0 r1 r2
--   => Record dr
--   -> Json
--   -> f (Builder {} (Record r2))
-- builderDecodeJsonWith decoderRecord = reportBuilderJson go
--   where
--   go :: Object Json -> f (Builder {} (Record r2))
--   go object = do
--     builder0 <-
--       __builderDecodeJsonWith
--         (RLProxy :: RLProxy l0)
--         (RLProxy :: RLProxy dl)
--         decoderRecord
--         object
--     record1 <- reportObject object (RLProxy :: RLProxy l1)
--     report $ (Builder.merge record1) <<< builder0
--
-- builderDecodeJsonWith_
--   :: forall dr dl f l0 l1 l2 r0 r1 r2
--    . Bind f
--   => BuilderDecodeJsonWith_ f dl dr l0 r0
--   => GDecodeJson r1 l1
--   => RowToList r1 l1
--   => RowToList r2 l2
--   => RowToList dr dl
--   => Status f
--   => Union r0 r1 r2
--   => Record dr
--   -> Json
--   -> f (Builder {} (Record r2))
-- builderDecodeJsonWith_ decoderRecord = reportBuilderJson go
--   where
--   go :: Object Json -> f (Builder {} (Record r2))
--   go object = do
--     builder0 <-
--       __builderDecodeJsonWith
--         (RLProxy :: RLProxy l0)
--         (RLProxy :: RLProxy dl)
--         decoderRecord
--         object
--     record1 <- reportObject object (RLProxy :: RLProxy l1)
--     report $ (Builder.union record1) <<< builder0
