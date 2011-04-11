module Typeable.Internal.Context where

import Typeable.T4e0b8f8ea2b145228fa4ec74b559bf6a --Class
import Typeable.T3e81531118e14888be21de7921b15bb5 --Type
import Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 --Definition

import Data.UUID
import Control.Monad.Reader
import Control.Monad.Error
import Typeable.Internal.Misc
import qualified Data.Map as M

type Context m a = ReaderT Static m a

data Static = Static { typeMap  :: M.Map UUID (Definition Type)
                     , classMap :: M.Map UUID (Definition Class)
                     }

runContext = runReaderT

getTypes  :: (Monad m) => Context m (M.Map UUID (Definition Type))
getTypes   = ask >>= return . typeMap

getType   :: (Monad m) => UUID -> Context m (Maybe (Definition Type))
getType u  = ask >>= return . (M.lookup u) . typeMap

instance HumanReadable UUID where
  humanify x = do s <- ask 
                  return $ case M.lookup x (typeMap s) of
                             Just a  -> show' $ name a
                             Nothing -> case M.lookup x (classMap s) of
                                          Just b  -> show' $ name b
                                          Nothing -> show' x 

class HumanReadable a where
  humanify :: (Monad m) => a -> Context m String
