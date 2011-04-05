{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.T34c13bdaac7d413ed735e64edcac7ff5 where
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
import Data.String
 
data Tuple (a :: *) (b :: *) = Tuple{fst :: a, snd :: b}
 
deriving instance (Prelude.Eq a, Prelude.Eq b) => Prelude.Eq
         (Tuple a b)
 
deriving instance (Prelude.Ord a, Prelude.Ord b) => Prelude.Ord
         (Tuple a b)
 
deriving instance (Prelude.Show a, Prelude.Show b) => Prelude.Show
         (Tuple a b)
 
instance (Data.EBF.EBF a, Data.EBF.EBF b, Data.EBF.TypeIdent a,
          Data.EBF.TypeIdent b) =>
         Data.EBF.EBF (Tuple a b) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 -> (>>=) Data.EBF.get (\ a1 -> return (Tuple a0 a1)))
        put (Tuple a b)
          = do Data.EBF.put a
               Data.EBF.put b
 
instance Data.Typeable.Typeable2 Tuple where
        typeOf2 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T34c13bdaac7d413ed735e64edcac7ff5.Tuple")
              []
 
instance Data.EBF.TypeIdentSS Tuple where
        typeOfSS _
          = Data.Tree.Node "34c13bda-ac7d-413e-d735-e64edcac7ff5" []