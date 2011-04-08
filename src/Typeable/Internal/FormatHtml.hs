{-# OPTIONS -XFlexibleInstances -XNoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Typeable.Internal.FormatHtml where

import Typeable.T9e2e1e478e094a8abe5507f8574ac91f --Succ
import Typeable.T421496848904471ea3197f25e2a02b72 --Zero
import Typeable.Tb0221a43509e4eddb062101bfd794bc4 --StructuredText
import Typeable.T2c62454c586f4bdea5e2b17e432db245 (Extension) --Extension 
import Typeable.Taf20e1db8f0d414f90625b1521e41378 --Language
import qualified Typeable.T9592f9fa4fae437a9e8d0917c14ff068 as TE --TextElement
import qualified Typeable.T1660b01f08dc4aedbe4c0941584541cb as K --Kind
import Typeable.T0174bd2264004820bfe34e211cb35a7d hiding (constraints)--DataType
import Typeable.T2a94a7a8d4e049759d8dd546e72293ff --Constraint
import qualified Typeable.T3819884685d34bf19b3469304e15983d as Person
import qualified Typeable.T205895c8d2df475b8d5ead5ee33d9f63 as Field
import qualified Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3 as Constructor
import qualified Typeable.Te590e9ce9cea4dfe86a413e9270dd1c2 as Method --Method
import qualified Typeable.T4e0b8f8ea2b145228fa4ec74b559bf6a as Class --Class
import qualified Typeable.T3e81531118e14888be21de7921b15bb5 as Type --Type
import Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 --Definition

import Text.Blaze
import Text.Blaze.Renderer.Utf8
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Prelude
import Numeric
import Data.Monoid   (mconcat, mempty)
import Data.Function
import Data.String
import Data.Char
import Data.UUID hiding (null)
import qualified Prelude              as P
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.ByteString.Lazy as BS

import Typeable.Internal.Context
import Typeable.Internal.InternalTypeDefs 

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

kind               :: (PeanoNumber a) => Type.Type a -> K.Kind
kind (Type.Quantification k x) = K.KindApplication k (kind x) 
kind _                         = K.KindStar
                              
kind'               :: (PeanoNumber a) => Class.Class a -> K.Kind
kind' (Class.Quantification k x) = K.KindApplication k (kind' x) 
kind' _                          = K.KindStar
                              
variables          :: K.Kind -> [Html]
variables x         = f 0 x
                      where
                        f _ K.KindStar          = []
                        f n (K.KindApplication k y) = ( H.span ! A.class_ "variable bound"
                                                          ! A.title  (stringValue $ show k)
                                                          $ string [toEnum ((fromEnum 'a') + n)]
                                                 ):( 
                                                   f (n+1) y 
                                                 )

class Htmlize a where
  htmlize :: (Monad m) => a -> (Context m Html)

instance Htmlize K.Kind where
  htmlize = htmlize' False
    where
      htmlize' _     K.KindStar          = return "\x2605" 
      htmlize' True  x                  = htmlize' False x >>= \y-> return $ do "("
                                                                                y
                                                                                ")"
      htmlize' False (K.KindApplication a b) = do a' <- htmlize' True  a
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
                 return $ H.ul $ do mconcat $ map (\(u,n)-> H.li $ H.a ! A.href (stringValue $ "class/"++(show' u)) $ H.span ! A.class_ "class" $ string n) cs'
                                    mconcat $ map (\(u,n)-> H.li $ H.a ! A.href (stringValue $ "type/"++(show' u))  $ H.span ! A.class_ "type"  $ string n) ts'
                                    mconcat $ map (\(f,s)-> H.li (string (show' f) >> s)) ns' 

    where
      ts = S.toList (nstypes   x)
      cs = S.toList (nsclasses x)
      ns = M.toList (subspaces x)

instance (PeanoNumber k) => Htmlize (StructuredText (Extension k)) where
  htmlize (Paragraph m) = let ls = M.findWithDefault [] ENG m
                          in  return $  if null ls then "" else string $ show $ TE.text $ head ls

instance (Htmlize k, PeanoNumber k) => Htmlize (Constraint k) where
  htmlize (Constraint i ts) = do ts' <- htmlize ts
                                 i'  <- humanify i
                                 return $ do H.a ! A.href (stringValue $ "../class/"++(show' i)) $ H.span ! A.class_ "class" $ string i'
                                             nbsp
                                             ts'

instance (PeanoNumber k, Htmlize k) => Htmlize (Field.Field k) where
  htmlize (Field.Field n s t) = do t' <- htmlize t
                                   s' <- htmlize s
                                   return $ H.tr $ do H.td ! A.class_ "function"   $ string (let g (x:xs)=(toLower x):xs in g $ show' n)
                                                      H.td ! A.class_ "type"       $ t'
                                                      H.td ! A.class_ "annotation" $ s'

instance (PeanoNumber k, Htmlize k) => Htmlize (Method.Method k) where
  htmlize (Method.Method n t s)
                         = do t' <- htmlize t
                              s' <- htmlize s
                              return $ H.tr $ do H.td ! A.class_ "function" $ string (let g (x:xs)=(toLower x):xs in g $ show' n)
                                                 H.td ! A.class_ "type"       $ t'
                                                 H.td ! A.class_ "annotation" $ s'

instance (PeanoNumber k, Htmlize k) => Htmlize (Constructor.Constructor k) where
  htmlize (Constructor.Constructor n s cs) = do s'  <- htmlize s
                                                cs' <- htmlize cs
                                                return $ do H.tr $ do H.td ! A.class_ "constructor" ! A.rowspan (stringValue $ show (P.length cs + 1)) $ string (show' n)
                                                                      H.td ! A.class_ "annotation"  ! A.colspan "3" $ s'
                                                            cs'

instance (PeanoNumber k, Htmlize k) => Htmlize (DataType k) where
  htmlize x = htmlize' x False
    where 
      htmlize' (DataType i) _ = humanify i >>= return . (H.a ! A.href (stringValue $ show' i)) . string
      htmlize' (Variable v) _ = htmlize v
      htmlize' (Forall _ t)   _ = do let vs = domain :: [k] 
                                     t' <- htmlize t 
                                     v' <- htmlize (Next $ last vs :: (Succ k))
                                     return $ do "\x2200"
                                                 v'
                                                 nbsp
                                                 t'
{--     htmlize' (Application
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
                                               --}
      htmlize' (Application t1 t2) w | t1 == DataType "0ba85f3f10099c75d4b696d0cf944e09" = do t2' <- htmlize t2
                                                                                              return $ do H.a ! A.href (stringValue $ show' $ reference t1) $ "["
                                                                                                          t2'
                                                                                                          H.a ! A.href (stringValue $ show' $ reference t1) $ "]"
                                     | t1 == DataType "7af30cce93724981a16a80f3f193dc33" = do t2' <- htmlize t2
                                                                                              return $ do H.a ! A.href (stringValue $ show' $ reference t1) $ "{"
                                                                                                          t2'
                                                                                                          H.a ! A.href (stringValue $ show' $ reference t1) $ "}"
                                     | otherwise = do t1' <- htmlize' t1 False
                                                      t2' <- htmlize' t2 True
                                                      return $ do if w then string "(" else mempty
                                                                  t1'
                                                                  nbsp
                                                                  t2'
                                                                  if w then string ")" else mempty

instance Htmlize UUID where
  htmlize x = return $ H.a ! A.href (stringValue $ (show' x)) ! A.class_ "fixedwidth" $ string (show' x)

instance Htmlize a => Htmlize [a] where
  htmlize xs = do xs' <- mapM htmlize xs
                  return $ mconcat xs'

instance Htmlize Person.Person where
  htmlize x  = return $ text $ Person.name x

instance Htmlize T.Text where
  htmlize = return . text

instance Htmlize (Definition Type.Type) where
  htmlize x  = do (semantics', constructors') <- toHtml (structure x)
                  kind' <- htmlize $ kind $ structure x
                  mp                                        <- metaPart x semantics'
                  return $     do mp
                                  H.table $ do
                                    H.tr $ H.td ! A.colspan "4" 
                                                ! A.class_ "caption" 
                                                $ do H.a ! A.href "" 
                                                         $ string $ show' (name x)
                                                     nbsp
                                                     mconcat (L.intersperse nbsp (variables $ kind $ structure x))
                                                     " :: "
                                                     kind'
                                    constructors'
    where
      toHtml               :: (PeanoNumber a, Htmlize a, Monad m) => Type.Type a -> Context m (Html, Html)
      toHtml (Type.Type s mc)
                            = do a  <- htmlize s 
                                 c  <- case mc of
                                         Nothing -> return $ H.tr $ H.td ! A.colspan "4" $ "This is an abstract type. Its possible values are described above."
                                         Just y  -> htmlize y
                                 return (a,c)
      toHtml (Type.Quantification _ x) = toHtml x

instance Htmlize (Definition Class.Class) where
  htmlize x  = do (semantics', constraints', methods') <- toHtml (structure x)
                  mp <- metaPart x semantics'
                  return $     do mp
                                  H.table $ do
                                    H.tr $ H.td ! A.colspan "3" 
                                                ! A.class_ "caption" 
                                                $ do H.a ! A.href "" 
                                                         $ H.span ! A.class_ "class" $ string $ show' (name x)
                                                     nbsp
                                                     mconcat (L.intersperse nbsp (variables $ kind' $ structure x))
                                    H.tr $ H.td ! A.colspan "3" ! A.class_ "constraints" $ constraints'
                                    methods'
    where
      toHtml               :: (PeanoNumber a, Htmlize a, Monad m) => Class.Class a -> Context m (Html, Html, Html)
      toHtml (Class.Quantification _ x) = toHtml x
      toHtml x              = do a  <- htmlize $ Class.semantics x 
                                 bs <- mapM htmlize $ S.toList $ Class.constraints x
                                 let b = mconcat $ L.intersperse (string "|") bs
                                 ms <- mapM htmlize (Class.methods x)
                                 return (a,b,mconcat ms)

metaPart  :: (Monad m) => Definition a -> Html -> Context m Html
metaPart x s = do author'              <- case author x of
                                            Nothing -> return "PublicDomain"
                                            Just a  -> htmlize a
                  maintainer'          <- htmlize (maintainer x)
                  return $ do
                    H.div ! A.id "meta" $ do
                      H.h1 "Meta"
                      H.table $ do H.tr $ do H.td "Name"
                                             H.td ! A.class_ "type" $ string $ show' $ name x
                                   H.tr $ do H.td "UUID"
                                             H.td $ string $ toString (identifier x)
                                   H.tr $ do H.td "Author"
                                             H.td author'
                                   H.tr $ do H.td "Maintainer"
                                             H.td maintainer' 
                                   H.tr $ do H.td "Created"
                                             H.td (string $ show' (creationTime x))
                                   H.tr $ do H.td "Modified"
                                             H.td (string $ show' (modificationTime x))
                    H.div ! A.id "exports" $ do
                      H.h1 "Export"
                      H.ul $ do H.li $ H.a ! A.href (stringValue ((show' (identifier x)) ++ "?format=haskell")) $ "Haskell" 
                                H.li $ H.a ! A.href (stringValue ((show' (identifier x)) ++ "?format=haskell-boot")) $ "Haskell-Boot" 
                                H.li $ H.a ! A.href (stringValue ((show' (identifier x)) ++ "?format=ebf")) $ "ExtensibleBinaryFormat" 
                    H.div ! A.id "description" $ do
                      H.h1 "Description"
                      H.div ! A.class_ "annotation" $ s
                    H.div ! A.id "structure" $ do
                      H.h1 "Structure"

