module Typeable.Internal.Graph where

import Data.Word

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S

type Graph a i = Reader (M.Map a (S.Set a)) i

runGraph = runReader

successors :: Ord a => a -> Graph a (S.Set a)
successors x = asks $ M.findWithDefault S.empty x 

successorsToDepth  :: Ord a => Word -> a -> Graph a (S.Set a)
successorsToDepth 0 _ = return S.empty
successorsToDepth i x = do ss  <- successors x 
                           sss <- mapM (successorsToDepth (i-1)) (S.toList ss)
                           return $ ss `S.union` foldl S.union S.empty sss




