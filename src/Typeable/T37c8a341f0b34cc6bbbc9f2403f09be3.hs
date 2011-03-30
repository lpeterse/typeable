{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.T205895c8d2df475b8d5ead5ee33d9f63
import qualified Typeable.T2c62454c586f4bdea5e2b17e432db245
import qualified Typeable.T9790ade9814a4aaca5eaa80c3e47685d
import qualified Typeable.Tb0221a43509e4eddb062101bfd794bc4
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
 
data Constructor (a :: *) = Constructor{name ::
                                        Typeable.T9790ade9814a4aaca5eaa80c3e47685d.Designator,
                                        semantics ::
                                        Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText
                                          (Typeable.T2c62454c586f4bdea5e2b17e432db245.Extension a),
                                        fields ::
                                        Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                                          (Typeable.T205895c8d2df475b8d5ead5ee33d9f63.Field a)}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Constructor a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Constructor a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Constructor a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (Constructor a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 ->
                                  (>>=) Typeable.Internal.EBF.get
                                    (\ a2 -> return (Constructor a0 a1 a2))))
        put (Constructor a b c)
          = do Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b
               Typeable.Internal.EBF.put c