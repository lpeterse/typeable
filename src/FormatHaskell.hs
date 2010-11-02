module FormatHaskell where

import Language.Haskell.Syntax
import Context
import InternalTypeDefs

typeDefinition2HsDataDecl :: (Kind a) => TypeDefinition a -> Context HsDecl
typeDefinition2HsDataDecl t = do 
                                 typeName <- upperDesignator2HsIdent $ name t
                                 let context      = []
                                 constructors <- case constructors t of
                                                   Just cs -> mapM constructor2HsConDecl cs
                                                   Nothing -> return []
                                 let derives      = []
                                 return $ HsDataDecl 
                                            (SrcLoc "" 0 0) 
                                            context 
                                            typeName
                                            []
                                            constructors
                                            derives

constructor2HsConDecl     :: (Kind a) => Constructor a    -> Context HsConDecl
constructor2HsConDecl c    = do let fs = constructorFields c 
                                let f x = do n <- lowerDesignator2HsIdent (fieldName x)
                                             return ([n], HsUnBangedTy (HsTyVar $ HsIdent "a"))
                                fs' <- mapM f fs
                                conName <- upperDesignator2HsIdent $ constructorName c
                                return $ HsRecDecl (SrcLoc "" 0 0) conName fs'

upperDesignator2HsIdent    :: UpperDesignator  -> Context HsName
upperDesignator2HsIdent x   = return $ HsIdent (show x)

lowerDesignator2HsIdent    :: LowerDesignator  -> Context HsName
lowerDesignator2HsIdent x   = return $ HsIdent (show x)
