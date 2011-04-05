{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T53e0d483a64144259dce752799d64305 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.Tf18ae792e5324a68a16f11ea5c61442a
import qualified Typeable.Td847a61a1a944723ab4bfcfb214bd8aa
import qualified Typeable.T1ea5eae4702844f7acbc3c65b2a40093
import qualified Typeable.Ted098cc975df4cd0adb99a5b7dc48600
 
data ContactInformation = Email{email ::
                                Typeable.T1ea5eae4702844f7acbc3c65b2a40093.UriByScheme
                                  Typeable.Ted098cc975df4cd0adb99a5b7dc48600.Mailto}
                        | Phone{phone ::
                                Typeable.T1ea5eae4702844f7acbc3c65b2a40093.UriByScheme
                                  Typeable.Tf18ae792e5324a68a16f11ea5c61442a.Tel}
                        | Website{website ::
                                  Typeable.T1ea5eae4702844f7acbc3c65b2a40093.UriByScheme
                                    Typeable.Td847a61a1a944723ab4bfcfb214bd8aa.Http}
 
deriving instance Prelude.Eq ContactInformation
 
deriving instance Prelude.Ord ContactInformation
 
deriving instance Prelude.Show ContactInformation
 
instance Data.EBF.EBF ContactInformation where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (Email a0))
                   1 -> (>>=) Data.EBF.get (\ a0 -> return (Phone a0))
                   2 -> (>>=) Data.EBF.get (\ a0 -> return (Website a0))
        put (Email a)
          = do Data.Binary.Put.putWord8 0
               Data.EBF.put a
        put (Phone a)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
        put (Website a)
          = do Data.Binary.Put.putWord8 2
               Data.EBF.put a
 
instance Data.Typeable.Typeable ContactInformation where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T53e0d483a64144259dce752799d64305")
              []