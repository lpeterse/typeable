{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T964f0abad1bd422faaf57b6d9938db31 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
 
data Tree (a :: *) = Tree{rootLabel :: a,
                          subForest ::
                          Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                            (Typeable.T964f0abad1bd422faaf57b6d9938db31.Tree a)}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Tree a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Tree a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Tree a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Tree a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 -> (>>=) Data.EBF.get (\ a1 -> return (Tree a0 a1)))
        put (Tree a b)
          = do Data.EBF.put a
               Data.EBF.put b