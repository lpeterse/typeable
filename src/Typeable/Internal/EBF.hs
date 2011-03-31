module Typeable.Internal.EBF where

import qualified Data.Binary as B
import qualified Data.Binary.Put as BP
import qualified Data.Binary.Get as BG
import qualified Data.Map    as M
import qualified Data.Set    as S
import Data.Text
import Data.Text.Encoding
import Data.ByteString
import Data.Word 
import Data.Int
import Data.Ratio

class (Ord a) => EBF a where
  put :: a -> B.Put
  get :: B.Get a

instance EBF Bool where
  put False = BP.putWord8 0
  put True  = BP.putWord8 1
  get       = BG.getWord8 >>= \x-> case x of
                                     0 -> return False
                                     1 -> return True

instance (EBF a) => EBF (Maybe a) where
        get
          = do index <- BG.getWord8
               case index of
                   0 -> return Nothing
                   1 -> (>>=) get (\ a0 -> return (Just a0))
        put Nothing = do BP.putWord8 0
        put (Just a)
          = do BP.putWord8 1
               put a

instance (EBF a) => EBF [a] where
  put []     = BP.putWord8 1
  put (x:xs) = BP.putWord8 0 >> put x >> put xs 
  get        = BG.getWord8 >>= \x-> case x of
                                      1 -> return []
                                      0 -> get >>= \a-> get >>= \b-> return (a:b)

instance (EBF a, EBF b) => EBF ((a,b)) where
  put (a,b) = put a >> put b
  get       = get >>= \a-> get >>= \b-> return (a,b)

instance (EBF a) => EBF (S.Set a) where
  put = put . S.toAscList
  get = get >>= (return . S.fromDistinctAscList)

instance (EBF k, EBF a) => EBF (M.Map k a) where
  put = put . M.toAscList 
  get = get >>= (return . M.fromDistinctAscList)

instance EBF ByteString where
  put = B.put
  get = B.get

instance EBF Integer where
  put = B.put
  get = B.get

instance EBF Text where
  put = put . encodeUtf8
  get = get >>= (return . decodeUtf8)

instance (Integral a, EBF a) => EBF (Ratio a) where
  put x = put (numerator x) >> put (denominator x)
  get   = get >>= \a-> get >>= \b-> return (a % b)

instance EBF Int where
  put = B.put
  get = B.get

instance EBF Word where
  put = B.put
  get = B.get

instance EBF Int8 where
  put = B.put
  get = B.get

instance EBF Int16 where
  put = B.put
  get = B.get

instance EBF Int32 where
  put = B.put
  get = B.get

instance EBF Int64 where
  put = B.put
  get = B.get

instance EBF Word8 where
  put = B.put
  get = B.get

instance EBF Word16 where
  put = B.put
  get = B.get

instance EBF Word32 where
  put = B.put
  get = B.get

instance EBF Word64 where
  put = B.put
  get = B.get

