{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T6716d098a58743379e54c12f249cdc0c where
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
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
data LatinAlphabet
 
instance Prelude.Eq LatinAlphabet
 
instance Prelude.Ord LatinAlphabet
 
instance Prelude.Show LatinAlphabet
 
instance Data.EBF.EBF LatinAlphabet
 
instance Data.Typeable.Typeable LatinAlphabet
 
instance Data.EBF.TypeIdent LatinAlphabet
 
instance Prelude.Enum LatinAlphabet