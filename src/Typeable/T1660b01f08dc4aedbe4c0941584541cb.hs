{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T1660b01f08dc4aedbe4c0941584541cb where
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
 
data Kind = KindStar{}
          | KindApplication{function ::
                            Typeable.T1660b01f08dc4aedbe4c0941584541cb.Kind,
                            argument :: Typeable.T1660b01f08dc4aedbe4c0941584541cb.Kind}
 
deriving instance Prelude.Eq Kind
 
deriving instance Prelude.Ord Kind
 
deriving instance Prelude.Show Kind
 
instance Data.EBF.EBF Kind where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return KindStar
                   1 -> (>>=) Data.EBF.get
                          (\ a0 ->
                             (>>=) Data.EBF.get (\ a1 -> return (KindApplication a0 a1)))
        put KindStar = do Data.Binary.Put.putWord8 0
        put (KindApplication a b)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
               Data.EBF.put b
 
instance Data.Typeable.Typeable Kind where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T1660b01f08dc4aedbe4c0941584541cb.Kind")
              []
 
instance Data.EBF.TypeIdent Kind where
        typeOf _ = Data.Tree.Node "1660b01f-08dc-4aed-be4c-0941584541cb" []