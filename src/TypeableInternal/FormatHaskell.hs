{-# LANGUAGE ScopedTypeVariables #-}
module TypeableInternal.FormatHaskell where

import Language.Haskell.Syntax
import TypeableInternal.Context
import TypeableInternal.InternalTypeDefs

import Typeable.Cb5ba7ec44dbb4236826c6ef6bc4837e4

typeDefinition2HsDataDecl :: forall k. (Kind k) => TypeDefinition k -> Context HsDecl
typeDefinition2HsDataDecl t = do typeName <- upperDesignator2HsIdent $ name t
                                 let context      = []
                                 let vars         = domain :: [k]
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

type2HsUnBangedTy                  :: (Kind a) => Type a -> Context HsType 
type2HsUnBangedTy (DataType u)     = do x <- humanify u
                                        return $ HsTyCon $ UnQual $ HsIdent $ x  
type2HsUnBangedTy (Application a b)   = do a' <- type2HsUnBangedTy a
                                           b' <- type2HsUnBangedTy b
                                           return $ HsTyApp a' b'  
type2HsUnBangedTy (Variable v) = return $ HsTyVar $ HsIdent $ var2String v 

