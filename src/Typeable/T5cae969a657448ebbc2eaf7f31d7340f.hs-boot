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
import qualified Data.EBF
 
data Triple (a :: *) (b :: *) (c :: *)
 
instance (Prelude.Eq a, Prelude.Eq b, Prelude.Eq c) => Prelude.Eq
         (Triple a b c)
 
instance (Prelude.Ord a, Prelude.Ord b, Prelude.Ord c) =>
         Prelude.Ord (Triple a b c)
 
instance (Prelude.Show a, Prelude.Show b, Prelude.Show c) =>
         Prelude.Show (Triple a b c)
 
instance (Data.EBF.EBF a, Data.EBF.EBF b, Data.EBF.EBF c) =>
         Data.EBF.EBF (Triple a b c)