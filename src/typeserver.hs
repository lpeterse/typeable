{-# LANGUAGE OverloadedStrings #-}
module Main where

import Happstack.Server 
import Happstack.State

import System                        (getArgs)
import System.IO.Unsafe
import IO
import System.Directory
import System.FilePath.Posix

import Data.EBF
import Data.UUID                     (UUID)
import Data.Monoid
import Data.Char
import Data.String
import Control.Monad
import qualified Data.Set             as S
import qualified Data.Map             as M
import qualified Data.ByteString.Lazy as LBS

import Language.Haskell.Exts.Syntax  (Module)
import Language.Haskell.Exts.Pretty

import Text.Parsec.String
import Text.Blaze
import Text.Blaze.Renderer.Utf8
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import Typeable.Internal.Classes
import Typeable.Internal.Context
import Typeable.Internal.FormatHtml
import Typeable.Internal.FormatHaskell
import Typeable.Internal.NamespaceParser
import Typeable.Internal.Misc
import Typeable.Internal.Manipulator

import Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 --Definition
import Typeable.T3e81531118e14888be21de7921b15bb5 --Type

main :: IO ()
main  = do args <- getArgs
           let p = if null args then 8000 else read (args !! 0)
           dc <- getDirectoryContents "static/types"
           let dc' = filter (\x->x `notElem` [".",".."]) dc
           ts <- mapM (\x->LBS.readFile $ "static/types" </> x) dc'
           let types  = map readV00 ts :: [Definition Type]
           let g z    = M.fromList (map (\x->(identifier x, x)) z)
           let static = Static (g types) M.empty
           bracket (startSystemState (Proxy :: Proxy AppState)) createCheckpointAndShutdown $
             \_control -> simpleHTTP (nullConf {port = p}) (msum $ handlers static) 
        where
          createCheckpointAndShutdown control = do createCheckpoint control
                                                   shutdownSystem control

namespace :: Namespace
namespace  = unsafePerformIO $ do n <- parseFromFile namespaceParser "static/default.namespace"
                                  case n of
                                    Left e -> error $ show e
                                    Right e -> return e

handlers :: Static -> [ServerPart Response]
handlers s = [ dirs "static/style.css" $ serveDirectory DisableBrowsing [] "static/style.css"  
             , dir  "type"   $ nullDir >> listTypes   s
             , dir  "type"   $ path $     serveType   s
             , dir  "data"   $ do i <- look "id"
                                  if all isHexDigit i && length i == 32
                                    then serveManipulator (fromString i)
                                    else mzero
             , dir "data"    $ ok $ toResponse ("please choose a datatype!" :: String)
             , serveOverview s
             ]

serveOverview :: Static -> ServerPartT IO Response
serveOverview s = runContext (htmlize namespace) s >>= ok . toResponse . encapsulate 

listTypes     s = ok $ toResponse $ unlines $ map show' $ M.keys (typeMap s)

serveType :: Static -> UUID -> ServerPartT IO Response
serveType s u  = case M.lookup u (typeMap s) of
                   Just t  -> do msum [ withDataFn (look "format") $ \x -> case x of
                                                                            "haskell" -> msum [ serveFile
                                                                                                  (asContentType "text/plain")
                                                                                                  ("static"</>"exports"</>"haskell"</>"T"++(show' u)<.>"hs") 
                                                                                              , runContext (typeModule False t) s >>= ok . toResponse 
                                                                                              ]
                                                                            "haskell-boot" -> msum [ serveFile
                                                                                                  (asContentType "text/plain")
                                                                                                  ("static"</>"exports"</>"haskell"</>"T"++(show' u)<.>"hs-boot") 
                                                                                              , runContext (typeModule True t) s >>= ok . toResponse 
                                                                                              ]
                                                                            "ebf"  -> (return $ writeV00 t) >>= ok . toResponseBS "text/plain" 
                                                                            "show" -> (return $ show t)     >>= ok . toResponse 
                                                                            _      -> mempty
                                      , runContext (htmlize t) s >>= ok . toResponse . encapsulate
                                      ]
                   Nothing -> notFound $ toResponse ((show' u)++" does not exist.") 

instance FromReqURI UUID where
  fromReqURI s = do a <- fromReqURI s :: Maybe String
                    return $ fromString a

instance ToMessage Module where
  toContentType _ = "text/plain"
  toMessage       = toMessage . prettyPrint


