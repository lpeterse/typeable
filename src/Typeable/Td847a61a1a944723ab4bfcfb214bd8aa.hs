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
import qualified Data.Tree
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
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
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 287485269114650100079668643524395129002)
              []
 
instance Prelude.Enum Http where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined