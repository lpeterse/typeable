{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T606f253533d3420da3465afae341d598 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.Tc211e54d6eef4234a7b675d5f696efe5
 
data Time (a :: *) = Time{seconds ::
                          Typeable.Tc211e54d6eef4234a7b675d5f696efe5.Rational}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Time a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Time a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Time a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (Time a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get (\ a0 -> return (Time a0))
        put (Time a) = do Typeable.Internal.EBF.put a