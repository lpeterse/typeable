{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.T26b9a53370bc4489a322192e2e0416ce where
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
 
data SimpleSpeaker
 
instance Prelude.Eq SimpleSpeaker
 
instance Prelude.Ord SimpleSpeaker
 
instance Prelude.Show SimpleSpeaker
 
instance Data.EBF.EBF SimpleSpeaker
 
instance Data.Typeable.Typeable SimpleSpeaker
 
instance Data.EBF.TypeIdent SimpleSpeaker