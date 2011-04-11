{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T9e2e1e478e094a8abe5507f8574ac91f where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Tree
import qualified Data.Data
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import Data.String
 
data Succ (a :: *) = First{}
                   | Next{previous :: a}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Succ a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Succ a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Succ a)
 
instance (Data.EBF.EBF a, Data.EBF.TypeIdent a) => Data.EBF.EBF
         (Succ a) where
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
          = Data.Tree.Node "9e2e1e47-8e09-4a8a-be55-07f8574ac91f" []