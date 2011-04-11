{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T2a94a7a8d4e049759d8dd546e72293ff where
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
import {-# SOURCE #-} qualified
       Typeable.T0174bd2264004820bfe34e211cb35a7d
import qualified Typeable.T346674042a7248b4a94abff0726d0c43
 
data Constraint (a :: *) = Constraint{class_ ::
                                      Typeable.T346674042a7248b4a94abff0726d0c43.UUID,
                                      tail :: Typeable.T0174bd2264004820bfe34e211cb35a7d.DataType a}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Constraint a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Constraint a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Constraint a)
 
instance (Data.EBF.EBF a, Data.EBF.TypeIdent a) => Data.EBF.EBF
         (Constraint a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 -> (>>=) Data.EBF.get (\ a1 -> return (Constraint a0 a1)))
        put (Constraint a b)
          = do Data.EBF.put a
               Data.EBF.put b
 
instance Data.Typeable.Typeable1 Constraint where
        typeOf1 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T2a94a7a8d4e049759d8dd546e72293ff.Constraint")
              []
 
instance Data.EBF.TypeIdentS Constraint where
        typeOfS _
          = Data.Tree.Node "2a94a7a8-d4e0-4975-9d8d-d546e72293ff" []