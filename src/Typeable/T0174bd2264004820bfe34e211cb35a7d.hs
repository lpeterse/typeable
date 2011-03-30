{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T0174bd2264004820bfe34e211cb35a7d where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import {-# SOURCE #-} qualified
       Typeable.T2a94a7a8d4e049759d8dd546e72293ff
import qualified Typeable.T7af30cce93724981a16a80f3f193dc33
import qualified Typeable.T346674042a7248b4a94abff0726d0c43
import qualified Typeable.T9e2e1e478e094a8abe5507f8574ac91f
 
data DataType (a :: *) = DataType{typeRef ::
                                  Typeable.T346674042a7248b4a94abff0726d0c43.UUID}
                       | Variable{variable :: a}
                       | Application{function ::
                                     Typeable.T0174bd2264004820bfe34e211cb35a7d.DataType a,
                                     argument ::
                                     Typeable.T0174bd2264004820bfe34e211cb35a7d.DataType a}
                       | Forall{constraints ::
                                Typeable.T7af30cce93724981a16a80f3f193dc33.Set
                                  (Typeable.T2a94a7a8d4e049759d8dd546e72293ff.Constraint
                                     (Typeable.T9e2e1e478e094a8abe5507f8574ac91f.Succ a)),
                                expression ::
                                Typeable.T0174bd2264004820bfe34e211cb35a7d.DataType
                                  (Typeable.T9e2e1e478e094a8abe5507f8574ac91f.Succ a)}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (DataType a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (DataType a)
 
deriving instance (Prelude.Show a) => Prelude.Show (DataType a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (DataType a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get (\ a0 -> return (DataType a0))
                   1 -> (>>=) Typeable.Internal.EBF.get (\ a0 -> return (Variable a0))
                   2 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 -> return (Application a0 a1)))
                   3 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get (\ a1 -> return (Forall a0 a1)))
        put (DataType a)
          = do Data.Binary.Put.putWord8 0
               Typeable.Internal.EBF.put a
        put (Variable a)
          = do Data.Binary.Put.putWord8 1
               Typeable.Internal.EBF.put a
        put (Application a b)
          = do Data.Binary.Put.putWord8 2
               Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b
        put (Forall a b)
          = do Data.Binary.Put.putWord8 3
               Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b