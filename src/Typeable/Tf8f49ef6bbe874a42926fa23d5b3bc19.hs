{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tf8f49ef6bbe874a42926fa23d5b3bc19 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Maybe (a :: *) = Nothing{}
                    | Just{just :: a}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Maybe a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Maybe a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Maybe a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (Maybe a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return Nothing
                   1 -> (>>=) Typeable.Internal.EBF.get (\ a0 -> return (Just a0))
        put Nothing = do Data.Binary.Put.putWord8 0
        put (Just a)
          = do Data.Binary.Put.putWord8 1
               Typeable.Internal.EBF.put a