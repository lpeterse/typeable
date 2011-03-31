{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T4e0b8f8ea2b145228fa4ec74b559bf6a where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.Te590e9ce9cea4dfe86a413e9270dd1c2
import qualified Typeable.T2a94a7a8d4e049759d8dd546e72293ff
import qualified Typeable.T7af30cce93724981a16a80f3f193dc33
import qualified Typeable.T2c62454c586f4bdea5e2b17e432db245
import qualified Typeable.Tb0221a43509e4eddb062101bfd794bc4
import qualified Typeable.T1660b01f08dc4aedbe4c0941584541cb
import qualified Typeable.T9e2e1e478e094a8abe5507f8574ac91f
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
 
data Class (a :: *) = Class{semantics ::
                            Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText
                              (Typeable.T2c62454c586f4bdea5e2b17e432db245.Extension a),
                            constraints ::
                            Typeable.T7af30cce93724981a16a80f3f193dc33.Set
                              (Typeable.T2a94a7a8d4e049759d8dd546e72293ff.Constraint a),
                            methods ::
                            Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                              (Typeable.Te590e9ce9cea4dfe86a413e9270dd1c2.Method a)}
                    | Quantification{kind ::
                                     Typeable.T1660b01f08dc4aedbe4c0941584541cb.Kind,
                                     quantified ::
                                     Typeable.T4e0b8f8ea2b145228fa4ec74b559bf6a.Class
                                       (Typeable.T9e2e1e478e094a8abe5507f8574ac91f.Succ a)}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Class a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Class a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Class a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (Class a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 ->
                                  (>>=) Typeable.Internal.EBF.get
                                    (\ a2 -> return (Class a0 a1 a2))))
                   1 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 -> return (Quantification a0 a1)))
        put (Class a b c)
          = do Data.Binary.Put.putWord8 0
               Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b
               Typeable.Internal.EBF.put c
        put (Quantification a b)
          = do Data.Binary.Put.putWord8 1
               Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b