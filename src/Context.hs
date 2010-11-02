module Context where

import InternalTypeDefs
import qualified Data.Map as M
import Control.Monad.Reader

type Context a = Reader (M.Map UUID String) a

runContext = runReader

class HumanReadable a where
  humanify :: a -> Context String

instance HumanReadable UUID where
  humanify x = do m <- ask 
                  return $ case M.lookup x m of
                             Nothing -> show x
                             Just y  -> y

--


