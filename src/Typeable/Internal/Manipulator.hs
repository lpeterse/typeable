{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
             MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS  -XQuasiQuotes #-}
module Typeable.Internal.Manipulator where

import Happstack.Server
import Happstack.State
import Data.Data
import Data.Tree
import Data.UUID
import Data.EBF
import Data.UUID.Quasi
import qualified Data.Map as M

data AppState = AppState { dates :: M.Map UUID Date
                         }
                deriving (Eq, Ord, Read, Show, Data, Typeable)

data Date     = Date
                { dateType :: Tree UUID
                , dateTree :: UntypedTree
                }
                deriving (Eq, Ord, Read, Show, Data, Typeable)

data UntypedTree = Undefined
                 | Algebraic Int [UntypedTree]
                 deriving (Eq, Ord, Read, Show, Data, Typeable)
 
instance Version AppState
instance Version UUID
instance Version Date
instance Version UntypedTree
instance (Version a) => Version (Tree a)
$(deriveSerialize ''AppState)
$(deriveSerialize ''Date)
$(deriveSerialize ''UUID)
$(deriveSerialize ''Tree)
$(deriveSerialize ''UntypedTree)

uuid1 = [uuid|0198ec39-0e3f-47c3-a231-6edb65c27c83|] 
uuid2 = [uuid|9900416f-e2cf-4ffe-a05a-278d3b12651c|]
uuid3 = [uuid|0219c59f-732a-8ef5-0721-5fbdb4cceacd|]

instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState $ M.singleton uuid1 (Date (Node uuid3 []) Undefined)

getDate  :: UUID -> Update AppState Date
getDate u = do as <- getState
               case M.lookup u (dates as) of
                 Nothing -> fail $ "there is no session " ++ (show u)
                 Just d  -> return d

putDate  :: UUID -> Date -> Update AppState ()
putDate u d = do as <- getState
                 putState $ as { dates = (M.insert u d $ dates as) }
                 
$(mkMethods ''AppState ['getDate, 'putDate])

serveManipulator  :: UUID -> ServerPart Response
serveManipulator u = do d <- update $ GetDate u 
                        ok $ toResponse $ show d


