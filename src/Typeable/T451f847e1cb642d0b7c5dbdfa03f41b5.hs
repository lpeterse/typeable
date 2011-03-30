{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.Tf8f49ef6bbe874a42926fa23d5b3bc19
import qualified Typeable.T3819884685d34bf19b3469304e15983d
import qualified Typeable.T421496848904471ea3197f25e2a02b72
import qualified Typeable.T606f253533d3420da3465afae341d598
import qualified Typeable.T9790ade9814a4aaca5eaa80c3e47685d
import qualified Typeable.T346674042a7248b4a94abff0726d0c43
import qualified Typeable.Tc1b1f6c722c2436fab3180146520814e
 
data Definition (a :: * -> *) = Definition{identifier ::
                                           Typeable.T346674042a7248b4a94abff0726d0c43.UUID,
                                           antecedent ::
                                           Typeable.Tf8f49ef6bbe874a42926fa23d5b3bc19.Maybe
                                             Typeable.T346674042a7248b4a94abff0726d0c43.UUID,
                                           name ::
                                           Typeable.T9790ade9814a4aaca5eaa80c3e47685d.Designator,
                                           creationTime ::
                                           Typeable.T606f253533d3420da3465afae341d598.Time
                                             Typeable.Tc1b1f6c722c2436fab3180146520814e.UTC,
                                           modificationTime ::
                                           Typeable.T606f253533d3420da3465afae341d598.Time
                                             Typeable.Tc1b1f6c722c2436fab3180146520814e.UTC,
                                           author ::
                                           Typeable.Tf8f49ef6bbe874a42926fa23d5b3bc19.Maybe
                                             Typeable.T3819884685d34bf19b3469304e15983d.Person,
                                           maintainer ::
                                           Typeable.T3819884685d34bf19b3469304e15983d.Person,
                                           structure ::
                                           a Typeable.T421496848904471ea3197f25e2a02b72.Zero}
 
deriving instance
         (Prelude.Eq (a Typeable.T421496848904471ea3197f25e2a02b72.Zero)) =>
         Prelude.Eq (Definition a)
 
deriving instance
         (Prelude.Ord
            (a Typeable.T421496848904471ea3197f25e2a02b72.Zero)) =>
         Prelude.Ord (Definition a)
 
deriving instance
         (Prelude.Show
            (a Typeable.T421496848904471ea3197f25e2a02b72.Zero)) =>
         Prelude.Show (Definition a)
 
instance (Typeable.Internal.EBF.EBF
            (a Typeable.T421496848904471ea3197f25e2a02b72.Zero)) =>
         Typeable.Internal.EBF.EBF (Definition a) where
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
                                         (\ a3 ->
                                            (>>=) Typeable.Internal.EBF.get
                                              (\ a4 ->
                                                 (>>=) Typeable.Internal.EBF.get
                                                   (\ a5 ->
                                                      (>>=) Typeable.Internal.EBF.get
                                                        (\ a6 ->
                                                           (>>=) Typeable.Internal.EBF.get
                                                             (\ a7 ->
                                                                return
                                                                  (Definition a0 a1 a2 a3 a4 a5 a6
                                                                     a7)))))))))
        put (Definition a b c d e f g h)
          = do Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b
               Typeable.Internal.EBF.put c
               Typeable.Internal.EBF.put d
               Typeable.Internal.EBF.put e
               Typeable.Internal.EBF.put f
               Typeable.Internal.EBF.put g
               Typeable.Internal.EBF.put h