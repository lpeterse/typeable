{-# OPTIONS -XFlexibleInstances -XNoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module TypeableInternal.FormatHtml where

import Typeable.Cb5ba7ec44dbb4236826c6ef6bc4837e4
import Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83
import Typeable.T421496848904471ea3197f25e2a02b72
import Typeable.T9e2e1e478e094a8abe5507f8574ac91f

import Prelude
import Data.String
import Data.List
import Numeric
import qualified Prelude as P
import Control.Monad (forM_)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text
import Data.Time.Calendar

import Text.Blaze.Renderer.Utf8

import qualified Data.ByteString.Lazy as L

import System.Process

import TypeableInternal.InternalTypeDefs
import qualified Network.URL as U
import Data.Monoid (mconcat, mempty)
import qualified Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import TypeableInternal.Context

encapsulate t = docTypeHtml $ do 
                  H.head $ do
                    H.title "typedoc"
                    H.link ! href (textValue "../static/style.css") ! rel (textValue "stylesheet") ! type_ (textValue "text/css")
                    H.link ! href (textValue "http://fonts.googleapis.com/css?family=Vollkorn") ! rel (textValue "stylesheet") ! type_ (textValue "text/css")
                  body t

class Htmlize a where
  htmlize :: a -> (Context Html)

show' :: (Show a) => a -> Text
show'  = pack . show

instance Htmlize Norm where
  htmlize x@(RFC n)  = return $ a (string $ show x) ! href (stringValue $ "http://tools.ietf.org/html/rfc" ++ show n)
  htmlize x@(ISO n)  = return $ a (string $ show x)
  htmlize x@(IEC n)  = return $ a (string $ show x)
  htmlize x@(DIN n)  = return $ a (string $ show x)
  htmlize x@(ECMA n) = return $ a (string $ show x)

instance Htmlize U.URL where
  htmlize x = return $ a (string $ show x) ! href (stringValue $ show x)

instance PeanoNumber k => Htmlize (Inline k) where
  htmlize (Plain t)       = return $         text t
  htmlize (Emph  t)       = return $ em     (text t) 
  htmlize (Strong t)      = return $ strong (text t)
  htmlize (Subscript t)   = return $ sub    (text t)
  htmlize (Superscript t) = return $ sup    (text t)
  htmlize (Monospace t)   = return $ code   (text t)
  htmlize (URL u)         = htmlize u
  htmlize (Norm n)        = htmlize n
  htmlize (Type t)        = undefined
  htmlize (Class i)       = undefined

instance (Htmlize k, PeanoNumber k) => Htmlize (Constraint k) where
  htmlize (Constraint i ts) = do ts' <- htmlize ts
                                 i'  <- humanify i
                                 return $ do H.a ! href (stringValue $ "../class/"++(show i)) $ H.span ! class_ "class" $ string i'
                                             preEscapedText "&nbsp;"
                                             ts'

instance PeanoNumber k => Htmlize (Annotation k) where
  htmlize (Block m)   | M.null m    = return mempty
                      | otherwise   = let f (l,c) = htmlize c >>= return . (H.div ! lang (stringValue (show l))) 
                                      in  mapM f (M.toList m) >>= return . mconcat
  htmlize (IndentList xs)           = do xs' <- mapM htmlize xs
                                         return $ H.div ! class_ "indent" $ mconcat $ xs'
  htmlize (BulletList xs)           = do xs' <- mapM htmlize xs
                                         return $ ul $ mconcat $ P.map li xs'
  htmlize (IndexedList xs)          = do xs' <- mapM htmlize xs
                                         return $ ol $ mconcat $ P.map li xs'
  htmlize (TitledList xs)           = undefined

instance (PeanoNumber k, Htmlize k) => Htmlize (Field k) where
  htmlize (Field n s t) = do t' <- htmlize t
                             s' <- htmlize s
                             return $ tr $ do td ! class_ "function"   $ string (show n)
                                              td ! class_ "type"       $ t'
                                              td ! class_ "annotation" $ s'

instance (PeanoNumber k, Htmlize k) => Htmlize (Constructor k) where
  htmlize (Constructor n s cs) = do s'  <- htmlize s
                                    cs' <- htmlize cs
                                    return $ do tr $ do td ! class_ "constructor" ! rowspan (stringValue $ show (P.length cs + 1)) $ string (show n)
                                                        td ! class_ "annotation"  ! colspan "3" $ s'
                                                cs'

instance (PeanoNumber k, Htmlize k) => Htmlize (Type k) where
  htmlize (DataType i)     = humanify i >>= return . (a ! href (stringValue $ show i)) . string
  htmlize (Variable v) = htmlize v
  htmlize (Application t1 t2) = do t1' <- htmlize t1
                                   t2' <- htmlize t2
                                   return $ do t1'
                                               preEscapedString "&nbsp;"
                                               string "("
                                               t2'
                                               string ")"

instance Htmlize Zero where
  htmlize = undefined

instance (PeanoNumber k) => Htmlize (Succ k) where
  htmlize x = return $ H.span ! class_ "variable bound" $ string $ return (toEnum (97 + fromEnum x) :: Char)

instance Htmlize UUID where
  htmlize x = return $ H.a ! href (stringValue $ (show x)) ! class_ "fixedwidth" $ string (show x)

instance Htmlize a => Htmlize [a] where
  htmlize xs = do xs' <- mapM htmlize xs
                  return $ mconcat xs'

instance (Htmlize k, PeanoNumber k) => Htmlize (TypeDefinition k) where
  htmlize x  = do a  <- htmlize (semantics x)
                  bs <- mapM htmlize $ S.toList (constraints x)
                  let bs' = Data.List.intersperse (string " | ") bs 
                  let b   = mconcat bs'
                  c  <- case constructors x of
                          Nothing -> return $ tr $ td ! colspan "4" $ "This is a primitive type. Its possible values are described above."
                          Just y  -> htmlize y
                  let vars = domain :: [k]
                  bv <- mapM htmlize vars
                  return $   do H.h2 "Meta"
                                H.div ! A.id "meta" $ do
                                  table $ do
                                    tr $ do
                                      td "Name"
                                      td ! class_ "type" $ (string $ show $ TypeableInternal.InternalTypeDefs.name x)
                                    tr $ do
                                      td "UUID"
                                      td $ string $ show (identifier x)
                                    tr $ do
                                      td "Author"
                                      td ""
                                    tr $ do
                                      td "Maintainer"
                                      td ""
                                    tr $ do 
                                      td "Created"
                                      td (string $ show (created x))
                                    tr $ do 
                                      td "Modified"
                                      td (string $ show (modified x))
                                H.h2 "Description"
                                H.div ! A.id "description" ! class_ "annotation" $ a
                                H.h2 "Structure"
                                H.div ! A.id "structure" $ do
                                  H.table $ do
                                    H.tr $ td ! colspan "4" ! class_ "type large" $ do string $ show (TypeableInternal.InternalTypeDefs.name x)
                                                                                       preEscapedString "&nbsp;"
                                                                                       mconcat (Data.List.intersperse (preEscapedString "&nbsp;") bv)
                                    H.tr $ td ! colspan "4" ! class_ "constraints" $ b
                                    c


---

