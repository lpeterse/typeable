{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.T421496848904471ea3197f25e2a02b72 where
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
 
data Zero
 
instance Prelude.Eq Zero
 
instance Prelude.Ord Zero
 
instance Prelude.Show Zero
 
instance Data.EBF.EBF Zero
 
instance Data.Typeable.Typeable Zero
 
instance Data.EBF.TypeIdent Zero
 
instance Prelude.Enum Zero