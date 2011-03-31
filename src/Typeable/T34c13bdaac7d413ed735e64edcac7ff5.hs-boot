{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T34c13bdaac7d413ed735e64edcac7ff5 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Tuple (a :: *) (b :: *)
 
instance (Prelude.Eq a, Prelude.Eq b) => Prelude.Eq (Tuple a b)
 
instance (Prelude.Ord a, Prelude.Ord b) => Prelude.Ord (Tuple a b)
 
instance (Prelude.Show a, Prelude.Show b) => Prelude.Show
         (Tuple a b)
 
instance (Data.EBF.EBF a, Data.EBF.EBF b) => Data.EBF.EBF
         (Tuple a b)