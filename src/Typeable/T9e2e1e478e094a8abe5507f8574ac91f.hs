{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T9e2e1e478e094a8abe5507f8574ac91f where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Tree
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
data Succ (a :: *) = First{}
                   | Next{previous :: a}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Succ a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Succ a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Succ a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Succ a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return First
                   1 -> (>>=) Data.EBF.get (\ a0 -> return (Next a0))
        put First = do Data.Binary.Put.putWord8 0
        put (Next a)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
 
instance Data.Typeable.Typeable1 Succ where
        typeOf1 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T9e2e1e478e094a8abe5507f8574ac91f.Succ")
              []
 
instance Data.EBF.TypeIdentS Succ where
        typeOfS _
          = Data.Tree.Node
              (UUID.UUID 210257483130955061794087428865177340191)
              []