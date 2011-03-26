{-# OPTIONS -XEmptyDataDecls -XStandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Typeable.T9e2e1e478e094a8abe5507f8574ac91f where

import Typeable.Cb5ba7ec44dbb4236826c6ef6bc4837e4 -- Finite
import {-# SOURCE #-} Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83 -- PeanoNumber 

data Succ a = First | Next a deriving (Eq, Ord, Show, Read)

instance PeanoNumber k => Enum (Succ k) where
  fromEnum First      = 0
  fromEnum (Next x)   = 1 + (fromEnum x)
  toEnum 0            = First
  toEnum n            = Next (toEnum (n-1))

instance (PeanoNumber a, Finite a) => Finite (Succ a) where
  domain = case domain :: [a] of
            [] -> [First]
            xs -> First:(map Next xs)
