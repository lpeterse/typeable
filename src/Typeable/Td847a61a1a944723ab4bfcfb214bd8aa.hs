{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Td847a61a1a944723ab4bfcfb214bd8aa where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Http
 
instance Prelude.Eq Http where
        (==) = undefined
 
instance Prelude.Ord Http where
        compare = undefined
 
instance Prelude.Show Http where
        show = undefined
 
instance Data.EBF.EBF Http where
        get = undefined
        put = undefined
 
instance Prelude.Enum Http where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined