{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T2a94a7a8d4e049759d8dd546e72293ff where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.T346674042a7248b4a94abff0726d0c43
import {-# SOURCE #-} qualified
       Typeable.T0174bd2264004820bfe34e211cb35a7d
 
data Constraint (a :: *) = Constraint{class_ ::
                                      Typeable.T346674042a7248b4a94abff0726d0c43.UUID,
                                      tail :: Typeable.T0174bd2264004820bfe34e211cb35a7d.DataType a}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Constraint a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Constraint a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Constraint a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (Constraint a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 -> return (Constraint a0 a1)))
        put (Constraint a b)
          = do Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b