{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tb6831ec097f14b8eba74b1e486b4175d where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.Tac2e770f2132aced749ec197385ff552
 
data Date (a :: *) = Time{days ::
                          Typeable.Tac2e770f2132aced749ec197385ff552.Int}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Date a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Date a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Date a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Date a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (Time a0))
        put (Time a) = do Data.EBF.put a
 
instance Data.Typeable.Typeable1 Date where
        typeOf1 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Tb6831ec097f14b8eba74b1e486b4175d")
              []