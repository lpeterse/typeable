module Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83 where

import Typeable.T421496848904471ea3197f25e2a02b72 
import Typeable.T9e2e1e478e094a8abe5507f8574ac91f 
import Typeable.Cb5ba7ec44dbb4236826c6ef6bc4837e4


class Finite a => PeanoNumber a

instance PeanoNumber Zero
instance PeanoNumber a => PeanoNumber (Succ a)
