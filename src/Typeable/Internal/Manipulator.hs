{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
             MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS  -XQuasiQuotes -XScopedTypeVariables #-}
module Typeable.Internal.Manipulator where

import Happstack.Server
import Happstack.State
import Data.Data
import Data.Tree
import Data.Char
import Data.UUID hiding (null)
import Data.EBF
import Data.UUID.Quasi
import Data.Monoid
import qualified Data.Map as M
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Text.Blaze
import qualified Text.Blaze.Html4.Transitional            as H
import qualified Text.Blaze.Html4.Transitional.Attributes as A

import Typeable.Internal.Context
import Typeable.Internal.Misc
import Typeable.Internal.FormatHtml

import qualified Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 as Definition
import qualified Typeable.T0174bd2264004820bfe34e211cb35a7d as DataType
import qualified Typeable.T421496848904471ea3197f25e2a02b72 as Zero
import qualified Typeable.T3e81531118e14888be21de7921b15bb5 as Type
import qualified Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3 as Constructor
import qualified Typeable.T205895c8d2df475b8d5ead5ee33d9f63 as Field
import qualified Typeable.T9e2e1e478e094a8abe5507f8574ac91f as Succ

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
uuid3 = [uuid|0219c59f-732a-8ef5-0721-5fbdb4cceacd|] -- bool
uuid4 = [uuid|f8f49ef6-bbe8-74a4-2926-fa23d5b3bc19|] -- maybe
uuid5 = [uuid|451f847e-1cb6-42d0-b7c5-dbdfa03f41b5|] -- Definition
uuid6 = [uuid|3e815311-18e1-4888-be21-de7921b15bb5|] -- Type

ttt   = Node
          [uuid|d9eef038-b47d-0820-c160-ceb8b6a89943|]
          [ Node uuid4 [Node uuid3 []]
          , Node [uuid|af20e1db-8f0d-414f-9062-5b1521e41378|] []
          ]

uuu   = Node uuid5 [Node uuid6 []]

instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState $ M.singleton uuid1 (Date uuu (Algebraic 0 [ Undefined, Undefined, Algebraic 0 [Algebraic 0 [], Undefined], Undefined, Undefined, Undefined, Undefined, Undefined]))

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


fillWith :: forall n. PeanoNumber n => DataType.DataType n -> DataType.DataType Zero.Zero -> DataType.DataType Zero.Zero
fillWith (DataType.DataType u)      _ = DataType.DataType u
fillWith (DataType.Application a b) x = DataType.Application (a `fillWith` x) (b `fillWith`x)
fillWith (DataType.Variable a)      x = let f (DataType.Application a b) 0 = b
                                            f (DataType.Application a b) i = f a (i-1)  
                                        in  f x $ (length $ (domain :: [n])) - (fromEnum a) - 1

visualize ::(Monad m) => DataType.DataType Zero.Zero -> UntypedTree -> Context m Html
visualize t Undefined = do t' <- htmlize t
                           return $ H.table
                                     ! A.cellpadding "0"
                                     ! A.cellspacing "0"
                                     $ H.tr
                                        $ H.td
                                           ! A.class_ "type"
                                           $ do H.div
                                                 ! A.class_ "tools"
                                                 $ H.span
                                                    ! A.class_ "button"
                                                    $ "►"
                                                H.div
                                                 ! A.class_ "typeName"
                                                 $ toHtml t'
visualize t (Algebraic i xs) = do let outer (DataType.DataType x)      = x
                                      outer (DataType.Application x y) = outer x
                                  td <- getType (outer t)
                                  t' <- htmlize t
                                  case td of
                                    Nothing -> fail "error4359: unknown type"
                                    Just z  -> do (k,fs) <- f (Definition.structure z)
                                                  return $ do 
                                                           H.table
                                                             ! A.cellpadding "0"
                                                             ! A.cellspacing "0"
                                                             $ do H.tr 
                                                                   $ do H.td 
                                                                         ! A.rowspan (toValue $ length xs)
                                                                         ! A.class_ "constructor" 
                                                                         $ do H.div
                                                                               ! A.class_  "tools"
                                                                               $ do H.span 
                                                                                     ! A.class_  "button"
                                                                                     ! A.onclick "toggle($(this).parent().parent().parent().parent().parent());"
                                                                                     $ "◄"
                                                                                    H.br
                                                                                    H.span
                                                                                     ! A.class_  "button"
                                                                                     $ "ℹ"
                                                                              H.div
                                                                               ! A.class_ "constructorName"
                                                                               $ k
                                                                        if null fs
                                                                          then mempty
                                                                          else head fs
                                                                  if null fs
                                                                    then mempty
                                                                    else mconcat $ map H.tr (tail fs)
                                                           H.table 
                                                             ! A.cellpadding "0"
                                                             ! A.cellspacing "0"
                                                             ! A.style  "display: none;"
                                                             $ H.tr
                                                                $ H.td
                                                                   ! A.class_ "type"
                                                                   $ do H.div
                                                                         ! A.class_ "tools"
                                                                         $ do H.span 
                                                                               ! A.class_  "button"
                                                                               ! A.onclick "toggle($(this).parent().parent().parent().parent().parent());"
                                                                               $ "►"
                                                                        H.div
                                                                         ! A.class_ "typeName"
                                                                         $ toHtml $ t'
                               where
                                  h :: (Monad m, PeanoNumber a) => Constructor.Constructor a -> Context m [Html]
                                  h x       = do ms <- mapM (\(al,fd)-> let tt = Field.type_ fd `fillWith` t
                                                                        in  do m <- visualize tt al 
                                                                               return $ do H.td 
                                                                                            ! A.class_ "function" 
                                                                                            $ (toHtml $ (\(z:zs)->(toLower z):zs) $ show' $ Field.name fd) 
                                                                                           H.td
                                                                                            ! A.class_ "type"
                                                                                            $ m
                                                            ) 
                                                            (zip xs $ Constructor.fields x) 
                                                 return $ ms
                                  cn :: [(Int, Constructor.Constructor a)] -> Html
                                  cn xs = H.select $ mconcat $ map 
                                            (\(l,c)->  let r = toHtml $ show' $ Constructor.name c
                                                       in  if i==l
                                                             then H.option ! A.selected "selected" $ r
                                                             else H.option r
                                            )
                                            xs
                                  g :: (Monad m, PeanoNumber a) => Maybe [Constructor.Constructor a] -> Context m (Html, [Html])
                                  g Nothing                   = return $ let a = toHtml ("abstract" :: String) in (a,[a])
                                  g (Just ys)                 = do a <- h (ys !! i) 
                                                                   return (cn $ zip [0..] ys, a)
                                  f :: (Monad m, PeanoNumber a) => Type.Type a -> Context m (Html, [Html])
                                  f (Type.Quantification k q) = f q
                                  f x                         = g (Type.constructors x)
                                  

convertType            :: Tree UUID -> DataType.DataType Zero.Zero 
convertType (Node u xs) = foldl DataType.Application (DataType.DataType u) (map convertType xs)

template :: Html -> Html
template t = H.docTypeHtml $ do
                  H.head $ do
                    H.title "typeable.org"
                    H.meta ! A.httpEquiv "Content-Type"
                           ! A.content "text/html; charset=utf-8"
                    H.link ! A.href "/static/jquery.sb.css"
                           ! A.rel "stylesheet"
                           ! A.type_ "text/css"
                    H.link ! A.href "/static/manipulator.css"
                           ! A.rel "stylesheet"
                           ! A.type_ "text/css"
                    H.script mempty ! A.src   "/static/jquery.js"
                                    ! A.type_ "text/javascript"
                    H.script mempty ! A.src   "/static/manipulator.js"
                                    ! A.type_ "text/javascript"
                    H.script mempty ! A.src   "/static/jquery.selectbox/jquery.sb.min.js"
                                    ! A.type_ "text/javascript"
                  H.body $ H.div ! A.style "border: 1px solid white;" $ t

