{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
             MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS  -XQuasiQuotes -XScopedTypeVariables #-}
module Typeable.Internal.Manipulator where

import Happstack.Server hiding (path)
import Happstack.State
import Data.Data hiding (constrIndex)
import Data.Tree
import Data.Char
import Data.Maybe
import Data.UUID hiding (null)
import Data.UUID.Quasi
import Data.Monoid
import qualified Data.IntMap as IM
import qualified Data.Map    as M
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Monad.Reader
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

data UntypedTree = Algebraic
                   { constrIndex :: Maybe Int                         -- The chosen constructor, Nothing denotes undefined
                   , fields      :: IM.IntMap (IM.IntMap UntypedTree) -- The outer map maps constructors, the inner one fields
                   , expanded    :: Bool                              -- denotes whether the constructor is expanded
                   }
                 deriving (Eq, Ord, Read, Show, Data, Typeable)

alg     :: Int -> (IM.IntMap UntypedTree) -> UntypedTree
alg i ls = Algebraic (Just i) (IM.singleton i ls) False

u       :: UntypedTree
u        = Algebraic Nothing IM.empty False

emptyTree = Algebraic Nothing IM.empty False

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


tt2   = Node
          [uuid|0ba85f3f-1009-9c75-d4b6-96d0cf944e09|]
          [ Node [uuid|6716d098-a587-4337-9e54-c12f249cdc0c|] []]

uuu   = Node uuid5 [Node uuid6 []]

instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState $ M.fromList    [ (uuid1, (Date uuu (alg 0 $ IM.fromList [(2,alg 0 $ IM.fromList [(0,alg 0 IM.empty)
                                                                                                           ,(1,alg 1 IM.empty)
                                                                                                           ]
                                                                                    )
                                                                                   ]
                                                              )
                                                     )
                                             )
                                          , (uuid2, (Date tt2 emptyTree))
                                          ]

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
serveManipulator s u = do d     <- update $ GetDate u 
                          let st = SessionStateS
                                    []
                                    (dateType d) 
                                    (dateTree d)
                                    (dateType d)
                                    (dateTree d)
                          msum [ do methodM POST
                                    decodeBody (defaultBodyPolicy "/tmp" 0 1000 1000)
                                    action <- look "action"
                                    case action of
                                      "setConstructor" -> do p <- look "path"
                                                             i <- look "index"
                                                             let (value, state) = runIdentity $ runContext (runStateT (setConstructor (read p) (read i)) st) s
                                                             update $ PutDate u $ d { dateTree = subTree state }
                                                             ok $ toResponse value
                                      _                -> fail "error8274: unknown command"
                               , ok $ toResponse $ template $ runIdentity $ runContext ( evalStateT visualize st ) s
                               ]

fillWith :: forall n. PeanoNumber n => DataType.DataType n -> DataType.DataType Zero.Zero -> DataType.DataType Zero.Zero
fillWith (DataType.DataType u)      _ = DataType.DataType u
fillWith (DataType.Application a b) x = DataType.Application (a `fillWith` x) (b `fillWith`x)
fillWith (DataType.Variable a)      x = let f (DataType.Application a b) 0 = b
                                            f (DataType.Application a b) i = f a (i-1)  
                                        in  f x $ (length $ (domain :: [n])) - (fromEnum a) - 1

treeTypeToListType                           :: PeanoNumber n => DataType.DataType n -> Tree UUID
treeTypeToListType (DataType.DataType u)      = Node u []
treeTypeToListType (DataType.Application a b) = let Node u ls = treeTypeToListType a
                                                in  Node u $ ls ++ [treeTypeToListType b]

listTypeToTreeType                           :: PeanoNumber n => Tree UUID -> DataType.DataType n
listTypeToTreeType (Node u ls)                = foldl DataType.Application (DataType.DataType u) (map listTypeToTreeType ls) 

----------------------------
-- SessionState Monad

type SessionState m a = StateT SessionStateS m a

data SessionStateS    = SessionStateS
                      { path     :: [(Int, Int)]
                      , subType  :: Tree UUID
                      , subTree  :: UntypedTree
                      , rootType :: Tree UUID
                      , rootTree :: UntypedTree
                      }

type Path = [(Int, Int)]

getPath     :: (Monad m) => SessionState m [(Int, Int)]
getPath      = get >>= (return . path)

setPath     :: (Monad m) => [(Int, Int)] -> SessionState m ()
setPath p    = modify (\x-> x { path = p })

getSubType  :: (Monad m) => SessionState m (Tree UUID)
getSubType   = gets subType

getSubTree  :: Monad m => SessionState m UntypedTree
getSubTree   = gets subTree

setSubTree  :: Monad m => UntypedTree -> SessionState m ()
setSubTree t = modify (\x-> x { subTree = t })

-- gets the fields in the current branch if constructor index is not undefined
getFields   :: Monad m => SessionState m (Maybe (IM.IntMap UntypedTree))
getFields    = do st <- getSubTree
                  case constrIndex st of
                    Nothing -> return Nothing
                    Just ci -> return $ IM.lookup ci (fields st)

-- going down the tree choosing a constructor and a field
branch      :: Monad m => (Int,Int) -> SessionState (ReaderT Static m) ()
branch (i,j) = do Node u ls <- gets subType
                  st        <- gets subTree
                  rt        <- gets subType
                  td        <- lift $ getType u
                  modify (\x-> x { path    = (i,j):(path x)
                                 , subType = f rt (Definition.structure $ fromJust td) 
                                 , subTree = IM.findWithDefault emptyTree j (IM.findWithDefault IM.empty i $ fields st)
                                 } )
             where
               f                            :: PeanoNumber a => Tree UUID -> Type.Type a -> Tree UUID
               f t (Type.Quantification _ q) = f t q
               f t x                         = g t (Type.constructors x)  
               g                            :: PeanoNumber a => Tree UUID -> Maybe [Constructor.Constructor a] -> Tree UUID
               g t Nothing                   = error "error7329: can't branch an abstract type"
               g t (Just ys)                 = if length ys < i || i < 0
                                                 then error "error4953: invalid constructor index"
                                                 else let fs = Constructor.fields (ys !! i) 
                                                      in  if length fs < j || j < 0
                                                            then error "error9231: invalid field index"
                                                            else treeTypeToListType $ Field.type_ (fs !! j) `fillWith` (listTypeToTreeType t)

-- creating a temporary context within that the given action is performed. path is rewinded, changes to the tree persist, root tree is never changed
branchLocal    :: Monad m => (Int,Int) -> SessionState (ReaderT Static m) a -> SessionState (ReaderT Static m) a
branchLocal i@(j,k) m = do s <- get 
                           branch i
                           x <- m
                           subSubTree <- getSubTree
                           let oldSubTree = subTree s
                           let newSubTree = oldSubTree { fields = IM.unionWith IM.union (IM.singleton j (IM.singleton k subSubTree)) (fields oldSubTree) }
                           modify (\x-> x { subType = (subType s), path = (path s), subTree = newSubTree })
                           return x

pathLocal         :: Monad m => Path -> SessionState (ReaderT Static m) a -> SessionState (ReaderT Static m) a
pathLocal []     a = a
pathLocal (x:xs) a = branchLocal x $ pathLocal xs a

-- sets the context according to path, changes the constructor index and returns a visualization of the altered subtree
setConstructor    :: Monad m => Path -> Int -> SessionState (ReaderT Static m) Html
setConstructor p i = pathLocal (reverse p) $ do st <- getSubTree
                                                setSubTree $ st { constrIndex = if i == -1 then Nothing else Just i }
                                                visualize

----------------------------
-- Html generation

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
                  H.body $ H.div ! A.id "treeContainer" $ t

visualize :: Monad m => SessionState (ReaderT Static m) Html
visualize  = do st <- getSubTree 
                t  <- getSubType
                t' <- lift $ htmlize (listTypeToTreeType t :: DataType.DataType Zero.Zero)
                let i = constrIndex st
                td <- lift $ getType $ rootLabel t
                p  <- getPath
                case td of
                  Nothing -> error "error4359: unknown type"
                  Just z  -> do (k,fs) <- f (Definition.structure z)
                                return $ do 
                                         H.table
                                           ! ( if i == Nothing
                                                 then A.class_ "contentUndefined"
                                                 else A.class_ ""
                                             )
                                           ! A.cellpadding "0"
                                           ! A.cellspacing "0"
                                           ! ( if i == Nothing
                                                 then A.style "display: none;"
                                                 else A.style mempty
                                             )
                                           $ do H.tr 
                                                 $ do H.td 
                                                       ! A.rowspan (toValue $ length fs)
                                                       ! A.class_ (toValue $ "constructorTools" ++ (if i == Nothing then " undefined" else "" :: String)) 
                                                       $ do H.span 
                                                             ! A.class_  (if i == Nothing
                                                                            then "button undefined"
                                                                            else "button"
                                                                         )
                                                             ! A.onclick "toggle($(this).parent().parent().parent().parent());"
                                                             $ "◄"
                                                            H.span
                                                             ! A.class_  "button"
                                                             ! A.onclick (toValue $ "alert(\""++(show $ reverse p)++"\");")
                                                             $ "ℹ"
                                                      H.td
                                                       ! A.rowspan (toValue $ length fs)
                                                       ! A.class_ (if i == Nothing
                                                                     then "constructorName undefined"
                                                                     else "constructorName"
                                                                  )
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
                                           ! ( if i /= Nothing
                                                 then A.style  "display: none;"
                                                 else A.style mempty
                                             )
                                           $ H.tr
                                              $ do H.td
                                                    ! A.class_ "typeTools"
                                                    $ if i == Nothing 
                                                        then H.span
                                                              ! A.class_  "button undefined"
                                                              ! A.onclick "toggle($(this).parent().parent().parent().parent());"
                                                              $ "►"
                                                        else H.span 
                                                              ! A.class_  "button"
                                                              ! A.onclick "toggle($(this).parent().parent().parent().parent());"
                                                              $ "►"
                                                   H.td
                                                    ! A.class_ "typeName"
                                                    $ toHtml $ t'
                  where
                     h        :: (Monad m, PeanoNumber a) => Constructor.Constructor a -> SessionState (ReaderT Static m) [Html]
                     h x       = do tr <- getSubTree
                                    ms <- mapM (\(al,fd)-> do m <- branchLocal (fromJust $ constrIndex tr,  al) $ visualize 
                                                              return $ do H.td 
                                                                           ! A.class_ "function" 
                                                                           $ (toHtml $ (\(z:zs)->(toLower z):zs) $ show' $ Field.name fd) 
                                                                          H.td
                                                                           ! A.class_ "type"
                                                                           $ m
                                               ) 
                                               (zip [0..] $ Constructor.fields x) 
                                    return ms
                     -- renders a selectbox for choosing a constructor. current constructor is preselected
                     cn :: (Monad m, PeanoNumber a) => [Constructor.Constructor a] -> SessionState (ReaderT Static m) Html
                     cn xs = do st <- getSubTree
                                p  <- getPath
                                return $ H.select 
                                          ! A.onchange (toValue $ "setConstructor($(this).parent().parent().parent().parent().parent(), '"++(show p)++"', this.value);")
                                          $ mconcat $ ((H.option ! A.value "-1" $ "undefined"):)
                                                    $ map 
                                                                (\(i,c)->  let r = toHtml $ show' $ Constructor.name c
                                                                           in  if Just i == constrIndex st
                                                                                 then H.option ! A.value (toValue i) ! A.selected "selected" $ r
                                                                                 else H.option ! A.value (toValue i) $ r
                                                                )
                                                                (zip [0..] xs)
                     -- returns the selectbox and the fields for the preselected entry
                     g :: (Monad m, PeanoNumber a) => Maybe [Constructor.Constructor a] -> SessionState (ReaderT Static m) (Html, [Html])
                     g Nothing                   = return ("abstract types aren't supported yet",[])
                     g (Just cs)                 = do st <- getSubTree
                                                      a  <- cn cs
                                                      case constrIndex st of
                                                        Nothing -> return (a,[])
                                                        Just i  -> if length cs < i || i < 0
                                                                     then error "error2419: constructor index out of range"
                                                                     else do b <- h (cs !! i) 
                                                                             return (a,b)
                     f :: (Monad m, PeanoNumber a) => Type.Type a -> SessionState (ReaderT Static m) (Html, [Html])
                     f (Type.Quantification _ q) = f q
                     f x                         = g (Type.constructors x)
                     


