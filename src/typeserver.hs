{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -XTypeSynonymInstances -XFlexibleInstances -XExistentialQuantification #-}
module Main where

import Typeable.T421496848904471ea3197f25e2a02b72

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

import Language.Haskell.Exts.Syntax (Module)
import Language.Haskell.Exts.Pretty

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
                                                                            "haskell" -> ok       $ toResponse $ typeModule $ unwrap t
                                                                            _         -> mempty
                                      , ok $ toResponse $ htmlize t
                                      ]
                   Nothing -> notFound $ toResponse ((show uuid)++" does not exist.") 


serveClass = serveType

-----------------

data Wrapped = WrappedType  { unwrap :: Definition Type' }
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

instance ToMessage (Context Module) where
  toContentType _ = "text/html"
  toMessage x     = toMessage $ prettyPrint $ runContext x nameMapping

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



