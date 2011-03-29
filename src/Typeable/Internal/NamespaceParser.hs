module Typeable.Internal.NamespaceParser where

import qualified Data.Set as S
import qualified Data.Map as M
import Typeable.Internal.InternalTypeDefs
import Data.String

import Text.ParserCombinators.Parsec 
type Perser a = CharParser () a
                           
indents :: Perser Int                          
indents = do  
           spaces <- (many (char ' '))
           return $ length spaces

emptyLine :: Perser String
emptyLine = try $ (char ' ') `manyTill` char '\n'

lineEnd   :: Parser String
lineEnd    = anyChar `manyTill` (eof <|> (char '\n' >> return ())) 


upperDesignator :: Perser UpperDesignator
upperDesignator = do
                    initial <- upper     
                    rest <- many $ choice [alphaNum, char '_']  
                    return $ fromString (initial:rest)
                         
lowerDesignator :: Perser LowerDesignator
lowerDesignator = do
                    initial <- lower     
                    rest <- many $ choice [alphaNum, char '_']  
                    return $ fromString (initial:rest)                         


parseUUID :: Perser UUID
parseUUID = do
              uuid' <- count 32 hexDigit
              return $ fromString uuid'

parseType :: Namespace -> Perser Namespace
parseType n = do
                string "t-" 
                uuid <- parseUUID
                lineEnd
                return n {nstypes = S.insert uuid (nstypes n)}
              
parseClass ::  Namespace -> Perser Namespace
parseClass n = do
                 string "c-" 
                 uuid <- parseUUID
                 lineEnd
                 return n {nsclasses = S.insert uuid (nsclasses n)}             

parseSubnamespace   :: Int -> Namespace -> Perser Namespace
parseSubnamespace i n = do ud <- upperDesignator
                           lineEnd
                           s  <- fillNamespace (i+2) $ Namespace {nstypes = S.empty, nsclasses = S.empty, subspaces = M.empty}
                           return n {subspaces = M.insert ud s $ subspaces n}

      
namespaceParser :: Perser Namespace
namespaceParser = fillNamespace 0 $ Namespace {nstypes = S.empty, nsclasses = S.empty, subspaces = M.empty}
      
      
              
fillNamespace :: Int -> Namespace -> Perser Namespace
fillNamespace i n = do many emptyLine
                       (eof >> return n) <|> do i' <- lookAhead indents
                                                case i' `compare` i of
                                                  EQ -> do indents
                                                           n' <- choice [parseType n, parseClass n, parseSubnamespace i n]
                                                           fillNamespace i n'                                        
                                                  LT -> return n
                                                  GT -> fail "check your indentation"
                           
