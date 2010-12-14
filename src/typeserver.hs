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
                                                                            "haskell" -> ok       $ toResponse $ haskellize t
                                                                            _         -> mempty
                                      , ok $ toResponse $ htmlize t
                                      ]
                   Nothing -> notFound $ toResponse ((show uuid)++" ist nicht vorhanden.") 
                 where
                   haskellize :: Wrapped -> String
                   haskellize (WrappedType t) = prettyPrint $ runContext (typeDefinition2HsDataDecl t) nameMapping 
                   haskellize _               = error "don't know how to haskellize classes"

serveClass = serveType




-----------------
data Wrapped = forall a. (Htmlize a, Kind a) => WrappedType   { unwrapType  :: (TypeDefinition  a) }
             | forall a. (Htmlize a, Kind a) => WrappedClass  { unwrapClass :: (ClassDefinition a) }

wrapType   :: (Htmlize a, Kind a) => TypeDefinition a -> (UUID, Wrapped)
wrapType  x = (identifier x, WrappedType x)

wrapClass  :: (Htmlize a, Kind a) => ClassDefinition a -> (UUID, Wrapped)
wrapClass x = (classIdentifier x , WrappedClass x)

records :: M.Map UUID Wrapped 
records  = M.fromList (types ++ classes)

nameMapping = M.map f records 
  where
    f (WrappedType x)  = show (name x)
    f (WrappedClass x) = show (className x)

instance Htmlize Wrapped where
  htmlize (WrappedType x)  = htmlize x
  htmlize (WrappedClass x) = htmlize x

instance FromReqURI UUID where
  fromReqURI s = do a <- fromReqURI s :: Maybe String
                    return $ fromString a

instance ToMessage Html where
  toContentType _ = "text/html"
  toMessage       = renderHtml . encapsulate

instance ToMessage (Context Html) where
  toContentType _ = "text/html"
  toMessage x     = renderHtml $ encapsulate $ runContext x nameMapping

classes :: [(UUID, Wrapped)]
classes = [
            wrapClass c1  -- Eq
          , wrapClass c2  -- Kind
          , wrapClass c3  -- Ord
          , wrapClass c4  -- Enum
          , wrapClass c5  -- Bounded

        --  , ("c6ebaa9f-4cdc-4068-894d-1ffaef5a7a83", "Kind")
        --  , ("0d864b18-19bd-4230-905b-bad04a4c195e", "Predicate")
        --  , ("882f4a6a-ffa2-4579-830e-0a850acad145", "TimeStandard")
        --  , ("edba1ef6-3e72-4b61-8256-9040555253a8", "AnnotationExtension")
          ]

types  :: [(UUID, Wrapped)]
types   = [  wrapType t1
           , wrapType t2
           , wrapType t3
           , wrapType t4
           , wrapType t5
           , wrapType t6
           , wrapType t7
           , wrapType t8
           , wrapType t9
           , wrapType t10
           , wrapType t11
           , wrapType t12
           , wrapType t13
           , wrapType t14
           , wrapType t15
           , wrapType t16
           , wrapType t17
           , wrapType t18
           , wrapType t19
           , wrapType t20
           , wrapType t21
           , wrapType t22
           , wrapType t23
           , wrapType t24
           , wrapType t25
           , wrapType t26
           , wrapType t27
           , wrapType t28
           , wrapType t29
           , wrapType t30
           , wrapType t31
           , wrapType t32
           , wrapType t33
           , wrapType t34
           , wrapType t35
           , wrapType t36
           , wrapType t37
           , wrapType t38
           , wrapType t39
           , wrapType t40
           , wrapType t41
           , wrapType t42
           , wrapType t43
           , wrapType t44
           , wrapType t45
           , wrapType t46
           , wrapType t47
           , wrapType t48
           , wrapType t49
           , wrapType t50
           , wrapType t51
           , wrapType t52
           , wrapType t53
           , wrapType t54
           , wrapType t55
           , wrapType t56
           , wrapType t57
           , wrapType t58
           , wrapType t59
           , wrapType t60
           , wrapType t61
           , wrapType t62
           , wrapType t63
           , wrapType t64
           , wrapType t65
           , wrapType t66
           , wrapType t67
           , wrapType t68
           , wrapType t69
           , wrapType t70
           , wrapType t71
           , wrapType t74
           , wrapType t75
           , wrapType t76
           , wrapType t78
           , wrapType t79
           , wrapType t80
           , wrapType t81
           , wrapType t82
           , wrapType t83
           , wrapType t84
           , wrapType t85
           , wrapType t86
           , wrapType t87
           , wrapType t90
           , wrapType t91
           , wrapType t92
           , wrapType t93 -- Ordering
        ]



