{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.Te590e9ce9cea4dfe86a413e9270dd1c2 where
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
import qualified Typeable.T0174bd2264004820bfe34e211cb35a7d
import qualified Typeable.T2c62454c586f4bdea5e2b17e432db245
import qualified Typeable.T9790ade9814a4aaca5eaa80c3e47685d
import qualified Typeable.Tb0221a43509e4eddb062101bfd794bc4
 
data Method (a :: *) = Method{name ::
                              Typeable.T9790ade9814a4aaca5eaa80c3e47685d.Designator,
                              signature :: Typeable.T0174bd2264004820bfe34e211cb35a7d.DataType a,
                              semantics ::
                              Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText
                                (Typeable.T2c62454c586f4bdea5e2b17e432db245.Extension a)}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Method a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Method a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Method a)
 
instance (Data.EBF.EBF a, Data.EBF.TypeIdent a) => Data.EBF.EBF
         (Method a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 ->
                             (>>=) Data.EBF.get
                               (\ a1 -> (>>=) Data.EBF.get (\ a2 -> return (Method a0 a1 a2))))
        put (Method a b c)
          = do Data.EBF.put a
               Data.EBF.put b
               Data.EBF.put c
 
instance Data.Typeable.Typeable1 Method where
        typeOf1 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Te590e9ce9cea4dfe86a413e9270dd1c2.Method")
              []
 
instance Data.EBF.TypeIdentS Method where
        typeOfS _
          = Data.Tree.Node "e590e9ce-9cea-4dfe-86a4-13e9270dd1c2" []