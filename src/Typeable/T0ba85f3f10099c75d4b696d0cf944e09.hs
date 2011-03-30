{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T0ba85f3f10099c75d4b696d0cf944e09 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data List (a :: *) = Nil{}
                   | Cons{head :: a,
                          tail :: Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List a}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (List a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (List a)
 
deriving instance (Prelude.Show a) => Prelude.Show (List a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (List a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return Nil
                   1 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get (\ a1 -> return (Cons a0 a1)))
        put Nil = do Data.Binary.Put.putWord8 0
        put (Cons a b)
          = do Data.Binary.Put.putWord8 1
               Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b