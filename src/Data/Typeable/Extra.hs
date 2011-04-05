{-# OPTIONS  -XStandaloneDeriving 
             -XNoMonomorphismRestriction 
             -XFlexibleInstances 
             -XKindSignatures 
             -XEmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typeable.Extra where

import Data.Typeable

data Dummy

typeOf_0       = typeOf1
typeOf_00      = typeOf2
typeOf_000     = typeOf3
typeOf_0000    = typeOf4
typeOf_00000   = typeOf5
typeOf_000000  = typeOf6
typeOf_0000000 = typeOf7

--------------------------------------------------------

class Typeable_1 t where
  typeOf_1 :: t (a :: * -> *) -> TypeRep

instance (Typeable_1 t, Typeable1 a) => Typeable (t a) where
  typeOf = typeOf_1_Default

typeOf_1_Default :: forall t a. (Typeable_1 t, Typeable1 a) => t a -> TypeRep
typeOf_1_Default = \_ -> rep 
 where
   rep = typeOf_1 (undefined :: t a) `mkAppTy` 
         typeOf1  (undefined :: a Dummy)

---------------------------------------------------------

class Typeable_10 t where
  typeOf_10 :: t (a :: * -> *) (b :: *) -> TypeRep

instance (Typeable_10 t, Typeable1 a) => Typeable1 (t a) where
  typeOf1 = typeOf_10_Default

typeOf_10_Default :: forall t a b. (Typeable_10 t, Typeable1 a) => t a b -> TypeRep
typeOf_10_Default = \_ -> rep 
 where
   rep = typeOf_10 (undefined :: t a b) `mkAppTy` 
         typeOf1   (undefined :: a Dummy)

---------------------------------------------------------

class Typeable_01 t where
  typeOf_01 :: t (a :: *) (b :: * -> *) -> TypeRep

instance (Typeable_01 t, Typeable a) => Typeable_1 (t a) where
  typeOf_1 = typeOf_01_Default

typeOf_01_Default :: forall t a b. (Typeable_01 t, Typeable a) => t a b -> TypeRep
typeOf_01_Default = \_ -> rep 
 where
   rep = typeOf_01 (undefined :: t a b) `mkAppTy` 
         typeOf    (undefined :: a)
