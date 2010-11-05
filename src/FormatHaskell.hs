{-# LANGUAGE ScopedTypeVariables #-}
module FormatHaskell where

import Language.Haskell.Syntax
import Context
import InternalTypeDefs

typeDefinition2HsDataDecl :: forall k . (Kind k) => TypeDefinition k -> Context HsDecl
typeDefinition2HsDataDecl t = do typeName <- upperDesignator2HsIdent $ name t
                                 let context      = []
                                 let vars         = varCount :: [k]
                                 constructors <- case constructors t of
                                                   Just cs -> mapM constructor2HsConDecl cs
                                                   Nothing -> return []
                                 let derives      = []
                                 return $ HsDataDecl 
                                            (SrcLoc "" 0 0) 
                                            context 
                                            typeName
                                            (map (HsIdent . var2String) vars)
                                            constructors
                                            derives

constructor2HsConDecl     :: (Kind a) => Constructor a    -> Context HsConDecl
constructor2HsConDecl c    = do let fs = constructorFields c 
                                let f x = do n  <- lowerDesignator2HsIdent (fieldName x)
                                             ty <- type2HsUnBangedTy (fieldType x) 
                                             return ([n], HsUnBangedTy ty)
                                fs' <- mapM f fs
                                conName <- upperDesignator2HsIdent $ constructorName c
                                return $ HsRecDecl (SrcLoc "" 0 0) conName fs'

upperDesignator2HsIdent    :: UpperDesignator  -> Context HsName
upperDesignator2HsIdent x   = return $ HsIdent (show x)

lowerDesignator2HsIdent    :: LowerDesignator  -> Context HsName
lowerDesignator2HsIdent x   = return $ HsIdent (show x)

type2HsUnBangedTy                  :: (Kind a) => Type a Void -> Context HsType 
type2HsUnBangedTy (Reference u)     = do x <- humanify u
                                         return $ HsTyCon $ UnQual $ HsIdent $ x  
type2HsUnBangedTy (Reduction a b)   = do a' <- type2HsUnBangedTy a
                                         b' <- type2HsUnBangedTy b
                                         return $ HsTyApp a' b'  
type2HsUnBangedTy (BoundVariable v) = return $ HsTyVar $ HsIdent $ var2String v 

var2String :: (Kind k) => k -> String
var2String x = [toEnum (97 + fromEnum x)]
