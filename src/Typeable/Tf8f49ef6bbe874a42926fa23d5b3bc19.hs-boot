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
import qualified Data.EBF
 
data Maybe (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Maybe a)
 
instance (Prelude.Ord a) => Prelude.Ord (Maybe a)
 
instance (Prelude.Show a) => Prelude.Show (Maybe a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Maybe a)