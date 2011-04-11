{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 where
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
 
data Definition (a :: * -> *)
 
instance Prelude.Eq (Definition a)
 
instance Prelude.Ord (Definition a)
 
instance Prelude.Show (Definition a)
 
instance (Data.EBF.TypeIdentS a) => Data.EBF.EBF (Definition a)
 
instance Data.Typeable.Extra.Typeable_1 Definition
 
instance Data.EBF.TypeIdentASS Definition