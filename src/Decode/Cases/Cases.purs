module Data.Argonaut.Decode.Cases where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class Cases
  (f :: Type -> Type)
  (l :: RowList)
  (r :: # Type)
  | l -> r

instance casesNil :: Cases f Nil ()

instance casesCons
  :: ( Cons s v r' r
     , Cases f l' r'
     )
  => Cases f (Cons s tv l') r
