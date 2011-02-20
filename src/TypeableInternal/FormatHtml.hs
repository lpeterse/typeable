{-# OPTIONS -XFlexibleInstances -XNoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module TypeableInternal.FormatHtml where

import Typeable.Cb5ba7ec44dbb4236826c6ef6bc4837e4
import Typeable.T421496848904471ea3197f25e2a02b72
import Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83
import Typeable.T9e2e1e478e094a8abe5507f8574ac91f

import Text.Blaze
import Text.Blaze.Renderer.Utf8
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Prelude
import Numeric
import Data.Monoid   (mconcat, mempty)
import Data.Function
import Data.String
import qualified Prelude              as P
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.ByteString.Lazy as BS

import TypeableInternal.Context
import TypeableInternal.InternalTypeDefs

encapsulate  :: Html -> Html
encapsulate t = H.docTypeHtml $ do 
                  H.head $ do
                    H.title              "typeable.org"
                    H.meta ! A.httpEquiv "Content-Type" 
                           ! A.content   "text/html; charset=utf-8"
                    H.link ! A.href      "../static/style.css"
                           ! A.rel       "stylesheet" 
                           ! A.type_     "text/css"
                    H.link ! A.href      "http://fonts.googleapis.com/css?family=Vollkorn" 
                           ! A.rel       "stylesheet"
                           ! A.type_     "text/css"
                  H.body t

nbsp = preEscapedString "&nbsp;"

kind               :: (PeanoNumber a) => Binding a Kind' c -> Kind'
kind (Expression _) = Concrete'
kind (Bind k x    ) = Application' k (kind x) 
                              
variables          :: Kind' -> [Html]
variables x         = f 0 x
                      where
                        f _ Concrete'          = []
                        f n (Application' k y) = ( H.span ! A.class_ "variable bound"
                                                          ! A.title  (stringValue $ show k)
                                                          $ string [toEnum ((fromEnum 'a') + n)]
                                                 ):( 
                                                   f (n+1) y 
                                                 )

class Htmlize a where
  htmlize :: (Monad m) => a -> (Context m Html)

instance Htmlize Kind' where
  htmlize = htmlize' False
    where
      htmlize' _     Concrete'          = return "\x2605" 
      htmlize' True  x                  = htmlize' False x >>= \y-> return $ do "("
                                                                                y
                                                                                ")"
      htmlize' False (Application' a b) = do a' <- htmlize' True  a
                                             b' <- htmlize' False b
                                             return $ do a'
                                                         "\x202f\x2192\x202f"
                                                         b'

instance Htmlize Zero where
  htmlize = undefined

instance PeanoNumber a => Htmlize (Succ a) where
  htmlize x = return $ H.span ! A.class_ "variable bound" $ string $ return (toEnum (97 + fromEnum x) :: Char)

instance Htmlize Namespace where
  htmlize x = do ns' <- mapM (htmlize . snd) ns >>= return . zip (map fst ns)
                 ts' <- mapM humanify ts        >>= return . L.sortBy (compare `on` snd) . zip ts
                 cs' <- mapM humanify cs        >>= return . L.sortBy (compare `on` snd) . zip cs
                 return $ H.ul $ do mconcat $ map (\(u,n)-> H.li $ H.a ! A.href (stringValue $ "class/"++(show u)) $ H.span ! A.class_ "class" $ string n) cs'
                                    mconcat $ map (\(u,n)-> H.li $ H.a ! A.href (stringValue $ "type/"++(show u))  $ H.span ! A.class_ "type"  $ string n) ts'
                                    mconcat $ map (\(f,s)-> H.li (string (show f) >> s)) ns' 

    where
      ts = S.toList (nstypes   x)
      cs = S.toList (nsclasses x)
      ns = M.toList (subspaces x)

instance PeanoNumber k => Htmlize (Annotation k) where
  htmlize (Plain t)     = return $ text t

instance (Htmlize k, PeanoNumber k) => Htmlize (Constraint k) where
  htmlize (Constraint i ts) = do ts' <- htmlize ts
                                 i'  <- humanify i
                                 return $ do H.a ! A.href (stringValue $ "../class/"++(show i)) $ H.span ! A.class_ "class" $ string i'
                                             nbsp
                                             ts'

instance (PeanoNumber k, Htmlize k) => Htmlize (Field k) where
  htmlize (Field n s t) = do t' <- htmlize t
                             s' <- htmlize s
                             return $ H.tr $ do H.td ! A.class_ "function"   $ string (show n)
                                                H.td ! A.class_ "type"       $ t'
                                                H.td ! A.class_ "annotation" $ s'

instance (PeanoNumber k, Htmlize k) => Htmlize (Method k) where
  htmlize (Method n t s) = do t' <- htmlize t
                              s' <- htmlize s
                              return $ H.tr $ do H.td ! A.class_ "function" $ string (show n)
                                                 H.td ! A.class_ "type"       $ t'
                                                 H.td ! A.class_ "annotation" $ s'

instance (PeanoNumber k, Htmlize k) => Htmlize (Constructor k) where
  htmlize (Constructor n s cs) = do s'  <- htmlize s
                                    cs' <- htmlize cs
                                    return $ do H.tr $ do H.td ! A.class_ "constructor" ! A.rowspan (stringValue $ show (P.length cs + 1)) $ string (show n)
                                                          H.td ! A.class_ "annotation"  ! A.colspan "3" $ s'
                                                cs'

instance (PeanoNumber k, Htmlize k) => Htmlize (Type k) where
  htmlize x = htmlize' x False
    where 
      htmlize' (DataType i) _ = humanify i >>= return . (H.a ! A.href (stringValue $ show i)) . string
      htmlize' (Variable v) _ = htmlize v
      htmlize' (Forall t)   _ = do let vs = domain :: [k] 
                                   t' <- htmlize t 
                                   v' <- htmlize (Next $ last vs :: (Succ k))
                                   return $ do "\x2200"
                                               v'
                                               nbsp
                                               t'
      htmlize' (Application
                 (Application 
                   (DataType (UUID 107557859644440974686449305308309621121)) 
                   t1
                 ) 
                 t2
               )            w = do t1' <- htmlize' t1 True 
                                   t2' <- htmlize  t2
                                   return $ do if w then "(" else ""
                                               t1'
                                               H.a ! A.href "/type/50eae3e85d2d42c88754b026cc360981" $ preEscapedString "&#x202F;\x2192&#x202F;"
                                               t2'
                                               if w then ")" else ""
      htmlize' (Application t1 t2) w | t1 == DataType "0ba85f3f10099c75d4b696d0cf944e09" = do t2' <- htmlize t2
                                                                                              return $ do H.a ! A.href (stringValue $ show $ typeRef t1) $ "["
                                                                                                          t2'
                                                                                                          H.a ! A.href (stringValue $ show $ typeRef t1) $ "]"
                                     | t1 == DataType "7af30cce93724981a16a80f3f193dc33" = do t2' <- htmlize t2
                                                                                              return $ do H.a ! A.href (stringValue $ show $ typeRef t1) $ "{"
                                                                                                          t2'
                                                                                                          H.a ! A.href (stringValue $ show $ typeRef t1) $ "}"
                                     | otherwise = do t1' <- htmlize' t1 False
                                                      t2' <- htmlize' t2 True
                                                      return $ do if w then string "(" else mempty
                                                                  t1'
                                                                  nbsp
                                                                  t2'
                                                                  if w then string ")" else mempty

instance Htmlize UUID where
  htmlize x = return $ H.a ! A.href (stringValue $ (show x)) ! A.class_ "fixedwidth" $ string (show x)

instance Htmlize a => Htmlize [a] where
  htmlize xs = do xs' <- mapM htmlize xs
                  return $ mconcat xs'

instance Htmlize Person where
  htmlize x  = return $ string $ personName x

instance Htmlize T.Text where
  htmlize = return . text

instance Htmlize (Definition Type') where
  htmlize x  = do (semantics', constraints', constructors') <- toHtml (structure x)
                  kind' <- htmlize $ kind $ structure x
                  mp                                        <- metaPart x semantics'
                  return $     do mp
                                  H.table $ do
                                    H.tr $ H.td ! A.colspan "4" 
                                                ! A.class_ "caption" 
                                                $ do H.a ! A.href "" 
                                                         $ string $ show (name x)
                                                     nbsp
                                                     mconcat (L.intersperse nbsp (variables $ kind $ structure x))
                                                     " :: "
                                                     kind'
                                    H.tr $ H.td ! A.colspan "4" ! A.class_ "constraints" $ constraints'
                                    constructors'
    where
      toHtml               :: (PeanoNumber a, Htmlize a, Monad m) => Binding a Kind' Type' -> Context m (Html, Html, Html)
      toHtml (Expression x) = do a  <- htmlize $ semantics x 
                                 bs <- mapM htmlize $ S.toList $ constraints x
                                 let b = mconcat $ L.intersperse (string "|") bs
                                 c  <- case constructors x of
                                         Nothing -> return $ H.tr $ H.td ! A.colspan "4" $ "This is an abstract type. Its possible values are described above."
                                         Just y  -> htmlize y
                                 return (a,b,c)
      toHtml (Bind _ x)     = toHtml x

instance Htmlize (Definition Class') where
  htmlize x  = do (semantics', constraints', methods') <- toHtml (structure x)
                  mp <- metaPart x semantics'
                  return $     do mp
                                  H.table $ do
                                    H.tr $ H.td ! A.colspan "3" 
                                                ! A.class_ "caption" 
                                                $ do H.a ! A.href "" 
                                                         $ H.span ! A.class_ "class" $ string $ show (name x)
                                                     nbsp
                                                     mconcat (L.intersperse nbsp (variables $ kind $ structure x))
                                    H.tr $ H.td ! A.colspan "3" ! A.class_ "constraints" $ constraints'
                                    methods'
    where
      toHtml               :: (PeanoNumber a, Htmlize a, Monad m) => Binding a Kind' Class' -> Context m (Html, Html, Html)
      toHtml (Expression x) = do a  <- htmlize $ classSemantics x 
                                 bs <- mapM htmlize $ S.toList $ classConstraints x
                                 let b = mconcat $ L.intersperse (string "|") bs
                                 ms <- mapM htmlize (classMethods x)
                                 return (a,b,mconcat ms)
      toHtml (Bind _ x)     = toHtml x

metaPart  :: (Monad m) => Definition a -> Html -> Context m Html
metaPart x s = do author'              <- case author x of
                                            Nothing -> return "PublicDomain"
                                            Just a  -> htmlize a
                  maintainer'          <- htmlize (maintainer x)
                  return $ do
                    H.div ! A.id "meta" $ do
                      H.h1 "Meta"
                      H.table $ do H.tr $ do H.td "Name"
                                             H.td ! A.class_ "type" $ string $ show $ name x
                                   H.tr $ do H.td "UUID"
                                             H.td $ string $ show (identifier x)
                                   H.tr $ do H.td "Author"
                                             H.td author'
                                   H.tr $ do H.td "Maintainer"
                                             H.td maintainer' 
                                   H.tr $ do H.td "Created"
                                             H.td (string $ show (creationTime x))
                                   H.tr $ do H.td "Modified"
                                             H.td (string $ show (modificationTime x))
                    H.div ! A.id "exports" $ do
                      H.h1 "Export"
                      H.ul $ do H.li $ H.a ! A.href (stringValue ((show (identifier x)) ++ "?format=haskell")) $ "Haskell" 
                                H.li $ H.a ! A.href (stringValue ((show (identifier x)) ++ "?format=haskell-boot")) $ "Haskell-Boot" 
                    H.div ! A.id "description" $ do
                      H.h1 "Description"
                      H.div ! A.class_ "annotation" $ s
                    H.div ! A.id "structure" $ do
                      H.h1 "Structure"

