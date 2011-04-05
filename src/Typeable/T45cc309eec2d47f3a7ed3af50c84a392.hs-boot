{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T45cc309eec2d47f3a7ed3af50c84a392 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data HexadecimalAlphabet
 
instance Prelude.Eq HexadecimalAlphabet
 
instance Prelude.Ord HexadecimalAlphabet
 
instance Prelude.Show HexadecimalAlphabet
 
instance Data.EBF.EBF HexadecimalAlphabet
 
instance Data.Typeable.Typeable HexadecimalAlphabet
 
instance Prelude.Enum HexadecimalAlphabet