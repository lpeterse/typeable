{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -XTypeSynonymInstances -XFlexibleInstances -XExistentialQuantification #-}
module Main where

--import Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83
import Typeable.T421496848904471ea3197f25e2a02b72
--import Typeable.T9e2e1e478e094a8abe5507f8574ac91f

import Happstack.Server
import Text.Blaze
import Text.Blaze.Renderer.Utf8
import Control.Monad
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import Data.String
import TypeableInternal.NamespaceParser
import Text.ParserCombinators.Parsec hiding (string)

import TypeableInternal.InternalTypeDefs
import TypeableInternal.Types
import TypeableInternal.Classes
import TypeableInternal.TypesDefault
import TypeableInternal.Context
import TypeableInternal.FormatHtml
import TypeableInternal.FormatHaskell
import System.IO.Unsafe
import System (getArgs)

import Language.Haskell.Pretty

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main  = do args <- getArgs
           let p = if null args then 8000 else read (args !! 0)
           simpleHTTP (nullConf {port = p}) (msum handlers) 


namespace :: Namespace
namespace  = unsafePerformIO $ do n <- parseFromFile namespaceParser "static/default.namespace"
                                  case n of
                                    Left e -> error $ show e
                                    Right e -> return e

handlers :: [ServerPart Response]
handlers  = [
              dir "static" $ fileServe ["style.css"] "static/"  
            , dir "type"   $ path serveType
            , dir "class"  $ path serveClass
            , serveOverview
            ]

serveOverview :: ServerPartT IO Response
serveOverview = ok $ toResponse $ (htmlize namespace :: Context Html)

serveType uuid = case M.lookup uuid records of
                   Just t  -> do msum [ withDataFn (look "format") $ \x -> case x of
                                                                            "haskell" -> undefined -- ok       $ toResponse $ haskellize t
                                                                            _         -> mempty
                                      , ok $ toResponse $ htmlize t
                                      ]
                   Nothing -> notFound $ toResponse ((show uuid)++" ist nicht vorhanden.") 


serveClass = serveType

-----------------

data Wrapped = WrappedType  (Definition Type')
             | WrappedClass (Definition Class')

records :: M.Map UUID Wrapped 
records  = M.fromList (map (f WrappedType) types ++ map (f WrappedClass) classes)
           where
             f z x = (identifier x, z x)

instance Htmlize Wrapped where
  htmlize (WrappedType x) = htmlize x
  htmlize (WrappedClass x) = htmlize x

nameMapping :: M.Map UUID String
nameMapping = M.map f records 
  where
    f (WrappedType x)  = show (name x)
    f (WrappedClass x) = show (name x)

instance FromReqURI UUID where
  fromReqURI s = do a <- fromReqURI s :: Maybe String
                    return $ fromString a

instance ToMessage Html where
  toContentType _ = "text/html"
  toMessage       = renderHtml . encapsulate

instance ToMessage (Context Html) where
  toContentType _ = "text/html"
  toMessage x     = renderHtml $ encapsulate $ runContext x nameMapping

classes :: [Definition Class']
classes = [
            c1  -- Eq
          , c2  -- Kind
          , c3  -- Ord
          , c4  -- Enum
          , c5  -- Bounded
          , c6  -- PeanoNumber
          , c7  -- TimeStandard
          , c8  -- Functor
          , c9  -- Applicative
          , c10 -- Monad
          , c11 -- Read
          , c12 -- Show
          ]

types  :: [Definition Type']
types   = [  t1
           , t2
           , t3
           , t4
           , t5
           , t6
           , t7
           , t8
           , t9
           , t10
           , t11
           , t12
           , t13
           , t14
           , t15
           , t16
           , t17
           , t18
           , t19
           , t20
           , t21
           , t22
           , t23
           , t24
           , t25
           , t26
           , t27
           , t28
           , t29
           , t31
           , t32
           , t33
           , t34
           , t35
           , t36
           , t37
           , t38
           , t39
           , t40
           , t41
           , t42
           , t43
           , t44
           , t45
           , t46
           , t47
           , t48
           , t49
           , t50
           , t51
           , t52
           , t53
           , t54
           , t55
           , t56
           , t57
           , t58
--           , t59
--           , t60
--           , t61
--           , t62
--           , t63
--           , t64
--           , t65
--           , t66
--           , t67
--           , t68
--           , t69
--           , t70
--           , t71
--           , t74
--           , t75
--           , t76
           , t80
           , t81
           , t82
           , t83
           , t84
           , t85
           , t86
           , t87
           , t90
           , t91
           , t92
           , t93 -- Ordering
           , t94 -- Extension
           , t95 -- Extension
           , t96 -- Quantification
           , t97 -- Kind
        ]



