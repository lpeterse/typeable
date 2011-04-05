{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.T0219c59f732a8ef507215fbdb4cceacd where
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
 
data Bool
 
instance Prelude.Eq Bool
 
instance Prelude.Ord Bool
 
instance Prelude.Show Bool
 
instance Data.EBF.EBF Bool
 
instance Data.Typeable.Typeable Bool
 
instance Data.EBF.TypeIdent Bool
 
instance Prelude.Enum Bool