{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T9592f9fa4fae437a9e8d0917c14ff068 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.T0219c59f732a8ef507215fbdb4cceacd
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
 
data TextElement (a :: *) = Text{text ::
                                 Typeable.T4f7db06c439541658a09689d3e7dd909.Text,
                                 bold :: Typeable.T0219c59f732a8ef507215fbdb4cceacd.Bool,
                                 italic :: Typeable.T0219c59f732a8ef507215fbdb4cceacd.Bool,
                                 monospace :: Typeable.T0219c59f732a8ef507215fbdb4cceacd.Bool,
                                 cancelled :: Typeable.T0219c59f732a8ef507215fbdb4cceacd.Bool}
                          | Extension{ext :: a}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (TextElement a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (TextElement a)
 
deriving instance (Prelude.Show a) => Prelude.Show (TextElement a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (TextElement a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 ->
                                  (>>=) Typeable.Internal.EBF.get
                                    (\ a2 ->
                                       (>>=) Typeable.Internal.EBF.get
                                         (\ a3 ->
                                            (>>=) Typeable.Internal.EBF.get
                                              (\ a4 -> return (Text a0 a1 a2 a3 a4))))))
                   1 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 -> return (Extension a0))
        put (Text a b c d e)
          = do Data.Binary.Put.putWord8 0
               Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b
               Typeable.Internal.EBF.put c
               Typeable.Internal.EBF.put d
               Typeable.Internal.EBF.put e
        put (Extension a)
          = do Data.Binary.Put.putWord8 1
               Typeable.Internal.EBF.put a