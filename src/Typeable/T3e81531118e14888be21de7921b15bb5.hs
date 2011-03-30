{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T3e81531118e14888be21de7921b15bb5 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.Tf8f49ef6bbe874a42926fa23d5b3bc19
import qualified Typeable.T2c62454c586f4bdea5e2b17e432db245
import qualified Typeable.Tb0221a43509e4eddb062101bfd794bc4
import qualified Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3
import qualified Typeable.T1660b01f08dc4aedbe4c0941584541cb
import qualified Typeable.T9e2e1e478e094a8abe5507f8574ac91f
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
 
data Type (a :: *) = Type{semantics ::
                          Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText
                            (Typeable.T2c62454c586f4bdea5e2b17e432db245.Extension a),
                          constructors ::
                          Typeable.Tf8f49ef6bbe874a42926fa23d5b3bc19.Maybe
                            (Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                               (Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3.Constructor a))}
                   | Quantification{kind ::
                                    Typeable.T1660b01f08dc4aedbe4c0941584541cb.Kind,
                                    quantified ::
                                    Typeable.T3e81531118e14888be21de7921b15bb5.Type
                                      (Typeable.T9e2e1e478e094a8abe5507f8574ac91f.Succ a)}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Type a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Type a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Type a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (Type a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get (\ a1 -> return (Type a0 a1)))
                   1 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 -> return (Quantification a0 a1)))
        put (Type a b)
          = do Data.Binary.Put.putWord8 0
               Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b
        put (Quantification a b)
          = do Data.Binary.Put.putWord8 1
               Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b