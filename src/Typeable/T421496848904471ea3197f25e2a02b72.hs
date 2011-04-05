{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
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
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
data Zero
 
instance Prelude.Eq Zero where
        (==) = undefined
 
instance Prelude.Ord Zero where
        compare = undefined
 
instance Prelude.Show Zero where
        show = undefined
 
instance Data.EBF.EBF Zero where
        get = undefined
        put = undefined
 
instance Data.Typeable.Typeable Zero where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T421496848904471ea3197f25e2a02b72.Zero")
              []
 
instance Data.EBF.TypeIdent Zero where
        typeOf _
          = Data.Tree.Node (UUID.UUID 87835946520937761699655933731964726130)
              []
 
instance Prelude.Enum Zero where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined