module Typeable.Cb5ba7ec44dbb4236826c6ef6bc4837e4 where

class (Enum a) => Finite a where
  domain :: [a]
  
