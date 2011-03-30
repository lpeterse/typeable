{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T1ea5eae4702844f7acbc3c65b2a40093 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
import qualified Typeable.T8068cbdaf35e4618a7e798c67ff9bee0
 
data UriByScheme (a :: *) = Uri{hierarchy ::
                                Typeable.T8068cbdaf35e4618a7e798c67ff9bee0.Hierarchy,
                                query :: Typeable.T4f7db06c439541658a09689d3e7dd909.Text,
                                fragment :: Typeable.T4f7db06c439541658a09689d3e7dd909.Text}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (UriByScheme a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (UriByScheme a)
 
deriving instance (Prelude.Show a) => Prelude.Show (UriByScheme a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (UriByScheme a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 ->
                                  (>>=) Typeable.Internal.EBF.get (\ a2 -> return (Uri a0 a1 a2))))
        put (Uri a b c)
          = do Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b
               Typeable.Internal.EBF.put c