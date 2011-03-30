{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Te393b15b944c4b3597cd02b1be6d693b where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
import qualified Typeable.T6e2f1233f1c84e6b9bb37c405c666234
import qualified Typeable.T8068cbdaf35e4618a7e798c67ff9bee0
 
data URI = URI{scheme ::
               Typeable.T6e2f1233f1c84e6b9bb37c405c666234.SchemeName,
               hierarchy :: Typeable.T8068cbdaf35e4618a7e798c67ff9bee0.Hierarchy,
               query :: Typeable.T4f7db06c439541658a09689d3e7dd909.Text,
               fragment :: Typeable.T4f7db06c439541658a09689d3e7dd909.Text}
 
deriving instance Prelude.Eq URI
 
deriving instance Prelude.Ord URI
 
deriving instance Prelude.Show URI
 
instance Typeable.Internal.EBF.EBF URI where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 ->
                                  (>>=) Typeable.Internal.EBF.get
                                    (\ a2 ->
                                       (>>=) Typeable.Internal.EBF.get
                                         (\ a3 -> return (URI a0 a1 a2 a3)))))
        put (URI a b c d)
          = do Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b
               Typeable.Internal.EBF.put c
               Typeable.Internal.EBF.put d