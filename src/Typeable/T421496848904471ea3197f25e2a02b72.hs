{-# OPTIONS -XEmptyDataDecls -XStandaloneDeriving #-}
module Typeable.T421496848904471ea3197f25e2a02b72 where

import Typeable.Cb5ba7ec44dbb4236826c6ef6bc4837e4 -- Finite

data Zero

instance Eq Zero where
  (==) = undefined
 
instance Ord Zero where
  compare = undefined

instance Show Zero where
  show = undefined

instance Enum Zero where
  fromEnum = undefined
  toEnum   = undefined

instance Finite Zero where
  domain = []

