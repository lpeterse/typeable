{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T964f0abad1bd422faaf57b6d9938db31 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))

import qualified Data.Tree
import qualified Data.EBF
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID

type Tree a = Data.Tree.Tree a

instance Data.EBF.TypeIdentS Data.Tree.Tree where
        typeOfS _
          = Data.Tree.Node (UUID.UUID 199794608445007063076168013328573586225) []
