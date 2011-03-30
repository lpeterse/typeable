{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T5cae969a657448ebbc2eaf7f31d7340f where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Triple (a :: *) (b :: *) (c :: *) = Triple{fst :: a, snd :: b,
                                                thrd :: c}
 
deriving instance (Prelude.Eq a, Prelude.Eq b, Prelude.Eq c) =>
         Prelude.Eq (Triple a b c)
 
deriving instance (Prelude.Ord a, Prelude.Ord b, Prelude.Ord c) =>
         Prelude.Ord (Triple a b c)
 
deriving instance
         (Prelude.Show a, Prelude.Show b, Prelude.Show c) => Prelude.Show
         (Triple a b c)
 
instance (Typeable.Internal.EBF.EBF a, Typeable.Internal.EBF.EBF b,
          Typeable.Internal.EBF.EBF c) =>
         Typeable.Internal.EBF.EBF (Triple a b c) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 ->
                                  (>>=) Typeable.Internal.EBF.get
                                    (\ a2 -> return (Triple a0 a1 a2))))
        put (Triple a b c)
          = do Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b
               Typeable.Internal.EBF.put c