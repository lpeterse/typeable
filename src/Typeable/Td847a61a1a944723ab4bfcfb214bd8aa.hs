{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.Td847a61a1a944723ab4bfcfb214bd8aa where
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
 
instance Data.Typeable.Typeable Http where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Td847a61a1a944723ab4bfcfb214bd8aa.Http")
              []
 
instance Data.EBF.TypeIdent Http where
        typeOf _ = Data.Tree.Node "d847a61a-1a94-4723-ab4b-fcfb214bd8aa" []
 
instance Prelude.Enum Http where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined