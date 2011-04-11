{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.Td9eef038b47d0820c160ceb8b6a89943 where
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
 
data Either (a :: *) (b :: *) = Left{left :: a}
                              | Right{right :: b}
 
deriving instance (Prelude.Eq a, Prelude.Eq b) => Prelude.Eq
         (Either a b)
 
deriving instance (Prelude.Ord a, Prelude.Ord b) => Prelude.Ord
         (Either a b)
 
deriving instance (Prelude.Show a, Prelude.Show b) => Prelude.Show
         (Either a b)
 
instance (Data.EBF.EBF a, Data.EBF.EBF b, Data.EBF.TypeIdent a,
          Data.EBF.TypeIdent b) =>
         Data.EBF.EBF (Either a b) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (Left a0))
                   1 -> (>>=) Data.EBF.get (\ a0 -> return (Right a0))
        put (Left a)
          = do Data.Binary.Put.putWord8 0
               Data.EBF.put a
        put (Right a)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
 
instance Data.Typeable.Typeable2 Either where
        typeOf2 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Td9eef038b47d0820c160ceb8b6a89943.Either")
              []
 
instance Data.EBF.TypeIdentSS Either where
        typeOfSS _
          = Data.Tree.Node "d9eef038-b47d-0820-c160-ceb8b6a89943" []