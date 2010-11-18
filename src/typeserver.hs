{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -XTypeSynonymInstances -XFlexibleInstances -XExistentialQuantification #-}
module Main where
--vandalismus!
import Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83
import Typeable.T421496848904471ea3197f25e2a02b72
import Typeable.T9e2e1e478e094a8abe5507f8574ac91f

import Happstack.Server
import Text.Blaze
import Text.Blaze.Renderer.Utf8
import Control.Monad
import Data.Monoid
import qualified Data.Map as M
import Data.String

import InternalTypeDefs
import Types
import TypesDefault
import Context
import FormatHtml
import FormatHaskell

import Language.Haskell.Pretty

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main  = simpleHTTP nullConf (msum handlers) 

handlers :: [ServerPart Response]
handlers  = [
              dir "static" $ fileServe ["style.css"] "static/"  
            , dir "type"   $ path serveType
            , dir "class"  $ path serveClass
            , serveOverview
            ]

serveType uuid = case M.lookup uuid typemap of
                   Just t  -> do msum [ withDataFn (look "format") $ \x -> case x of
                                                                            "haskell" -> ok       $ toResponse $ haskellize t
                                                                            _         -> mempty
                                      , ok $ toResponse $ htmlize t
                                      ]
                   Nothing -> notFound $ toResponse ((show uuid)++" ist nicht vorhanden.") 

serveClass uuid = let a = uuid :: UUID in notFound $ toResponse ("Classes aren't yet implemented." :: String)


serveOverview  = ok $ toResponse $ encapsulate ts
                 where
                   f (x, WrappedType t) = H.li $ H.a ! A.href (stringValue $ "type/"++(show x)) ! A.class_ "fixedwidth" $ (string $ show (name t))
                   ts  = H.ul $ mconcat (map f types)

--


data WrappedType = forall a. (Htmlize a, PeanoNumber a) => WrappedType { unwrap :: (TypeDefinition a) }

wrap  :: (Htmlize a, PeanoNumber a) => TypeDefinition a -> (UUID, WrappedType)
wrap x = (identifier x, WrappedType x)

typemap  :: M.Map UUID WrappedType 
typemap   = M.fromList types

instance Htmlize WrappedType where
  htmlize (WrappedType x) = htmlize x

haskellize :: WrappedType -> String
haskellize (WrappedType t) = prettyPrint $ runContext (typeDefinition2HsDataDecl t) nameMapping 

instance FromReqURI UUID where
  fromReqURI s = do a <- fromReqURI s :: Maybe String
                    return $ fromString a

instance ToMessage Html where
  toContentType _ = "text/html"
  toMessage       = renderHtml . encapsulate

classes :: [(UUID, String)]
classes  = [
             ("8658bc79-8a87-4218-9aa7-c70e2f9d0fe2", "Eq")
           , ("c6ebaa9f-4cdc-4068-894d-1ffaef5a7a83", "PeanoNumber")
           , ("0d864b18-19bd-4230-905b-bad04a4c195e", "Predicate")
           , ("882f4a6a-ffa2-4579-830e-0a850acad145", "TimeStandard")
           , ("edba1ef6-3e72-4b61-8256-9040555253a8", "AnnotationExtension")
           ]

nameMapping = M.fromList $ (map (\(x, WrappedType t)->(x, show (name t))) types) ++ classes

instance ToMessage (Context Html) where
  toContentType _ = "text/html"
  toMessage x     = renderHtml $ encapsulate $ runContext x $ nameMapping

types   = [  wrap t1
           , wrap t2
           , wrap t3
           , wrap t4
           , wrap t5
           , wrap t6
           , wrap t7
           , wrap t8
           , wrap t9
           , wrap t10
           , wrap t11
           , wrap t12
           , wrap t13
           , wrap t14
           , wrap t15
           , wrap t16
           , wrap t17
           , wrap t18
           , wrap t19
           , wrap t20
           , wrap t21
           , wrap t22
           , wrap t23
           , wrap t24
           , wrap t25
           , wrap t26
           , wrap t27
           , wrap t28
           , wrap t29
           , wrap t30
           , wrap t31
           , wrap t32
           , wrap t33
           , wrap t34
           , wrap t35
           , wrap t36
           , wrap t37
           , wrap t38
           , wrap t39
           , wrap t40
           , wrap t41
           , wrap t42
           , wrap t43
           , wrap t44
           , wrap t45
           , wrap t46
           , wrap t47
           , wrap t48
           , wrap t49
           , wrap t50
           , wrap t51
           , wrap t52
           , wrap t53
           , wrap t54
           , wrap t55
           , wrap t56
           , wrap t57
           , wrap t58
           , wrap t59
           , wrap t60
           , wrap t61
           , wrap t62
           , wrap t63
           , wrap t64
           , wrap t65
           , wrap t66
           , wrap t67
           , wrap t68
           , wrap t69
           , wrap t70
           , wrap t71
           , wrap t74
           , wrap t75
           , wrap t76
        ]



