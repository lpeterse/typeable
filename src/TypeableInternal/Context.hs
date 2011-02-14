module TypeableInternal.Context where

import TypeableInternal.InternalTypeDefs
import qualified Data.Map as M
import Control.Monad.Reader

type Context a = Reader Static a

data Static = Static { typeMap  :: M.Map UUID (Definition Type')
                     , classMap :: M.Map UUID (Definition Class')
                     }

runContext = runReader

class HumanReadable a where
  humanify :: a -> Context String

instance HumanReadable UUID where
  humanify x = do s <- ask 
                  return $ case M.lookup x (typeMap s) of
                             Just a  -> show $ name a
                             Nothing -> case M.lookup x (classMap s) of
                                          Just b  -> show $ name b
                                          Nothing -> show x 



