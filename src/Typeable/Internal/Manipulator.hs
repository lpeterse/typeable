{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
             MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS  -XQuasiQuotes -XScopedTypeVariables #-}
module Typeable.Internal.Manipulator where

import Happstack.Server
import Happstack.State
import Data.Data
import Data.Tree
import Data.UUID
import Data.EBF
import Data.UUID.Quasi
import Data.Monoid
import qualified Data.Map as M
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Text.Blaze
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import Typeable.Internal.Context
import Typeable.Internal.Misc
import Typeable.Internal.FormatHtml

import qualified Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 as Definition
import qualified Typeable.T0174bd2264004820bfe34e211cb35a7d as DataType
import qualified Typeable.T421496848904471ea3197f25e2a02b72 as Zero
import qualified Typeable.T3e81531118e14888be21de7921b15bb5 as Type
import qualified Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3 as Constructor

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
  initialValue = AppState $ M.singleton uuid1 (Date (Node uuid3 []) (Algebraic 0 [Undefined]))

getDate  :: UUID -> Update AppState Date
getDate u = do as <- getState
               case M.lookup u (dates as) of
                 Nothing -> fail $ "there is no session " ++ (show u)
                 Just d  -> return d

putDate  :: UUID -> Date -> Update AppState ()
putDate u d = do as <- getState
                 putState $ as { dates = (M.insert u d $ dates as) }
                 
$(mkMethods ''AppState ['getDate, 'putDate])

serveManipulator  :: Static -> UUID -> ServerPart Response
serveManipulator s u = do d <- update $ GetDate u 
                          ok $ toResponse $ template $ runIdentity $ runContext (visualize (convertType $ dateType d) (dateTree d)) s


visualize ::(Monad m) => DataType.DataType Zero.Zero -> UntypedTree -> Context m Html
visualize t Undefined = do t' <- htmlize t
                           return $ H.table 
                                      $ H.tr 
                                        $ H.td 
                                        ! A.class_ "type"
                                          $ toHtml $ t'
visualize t (Algebraic i xs) = do let outer (DataType.DataType x)      = x
                                      outer (DataType.Application x y) = outer x
                                  td <- getType (outer t)
                                  t' <- htmlize t
                                  case td of
                                    Nothing -> fail "error4534"
                                    Just z  -> return $ do H.table
                                                             ! A.onclick "toggle(this);"
                                                             $ H.tr 
                                                                 $ H.td 
                                                                 ! A.class_ "constructor" 
                                                                    $ f $ Definition.structure z
                                                        
                                                           H.table 
                                                             ! A.style   "display: none;"
                                                             ! A.onclick "toggle(this);"
                                                             $ H.tr 
                                                               $ H.td 
                                                                   ! A.class_ "type"
                                                                    $ toHtml $ t'
                                                        
                               where
                                  h :: (PeanoNumber a) => Constructor.Constructor a -> Html
                                  h x       = toHtml $ show' $ Constructor.name x
                                  g :: (PeanoNumber a) => Maybe [Constructor.Constructor a] -> Html
                                  g Nothing = toHtml ("abstract" :: String)
                                  g (Just ys)      = h (ys !! i) 
                                  f :: (PeanoNumber a) => Type.Type a -> Html
                                  f (Type.Quantification k q) = f q
                                  f x                         = g (Type.constructors x)
                                  


convertType :: Tree UUID -> DataType.DataType Zero.Zero 
convertType (Node u xs) = foldl DataType.Application (DataType.DataType u) (map convertType xs)

template :: Html -> Html
template t = H.docTypeHtml $ do
                  H.head $ do
                    H.title "typeable.org"
                    H.meta ! A.httpEquiv "Content-Type"
                           ! A.content "text/html; charset=utf-8"
                    H.link ! A.href "/static/manipulator.css"
                           ! A.rel "stylesheet"
                           ! A.type_ "text/css"
                    H.script mempty ! A.src   "/static/jquery.js"
                                    ! A.type_ "text/javascript"
                    H.script mempty ! A.src   "/static/manipulator.js"
                                    ! A.type_ "text/javascript"
                  H.body t

