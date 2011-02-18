module TypeableInternal.FormatHaskell where

import Data.List

import Language.Haskell.Exts.Syntax hiding (Context, Type, DataType)
import qualified Language.Haskell.Exts.Syntax as Syntax
import TypeableInternal.Context
import TypeableInternal.InternalTypeDefs

import Typeable.Cb5ba7ec44dbb4236826c6ef6bc4837e4
import Typeable.T421496848904471ea3197f25e2a02b72 -- Zero
import Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83

sl = SrcLoc "" 0 0

imports                          :: Decl -> [ImportDecl]
imports (DataDecl _ _ _ _ _ xs _) = concatMap f xs 
                                    where
                                    f (QualConDecl _ _ _ c) = g c
                                    g (RecDecl _ ts)        = concatMap h ts
                                    h (_, UnBangedTy t)     = i t
                                    i (TyVar _)             = []
                                    i (TyCon (Qual m _))    = [impDecl m]
                                    i (TyApp t1 t2)         = (i t1) ++ (i t2)

impDecl m = ImportDecl sl m False False Nothing Nothing Nothing

typeModule  :: Definition Type' -> Context Module
typeModule x = do dd <- dataDecl x
                  ib <- instanceBinary dd
                  let imps  = [ 
                                impDecl (ModuleName "Data.Binary")
                              , impDecl (ModuleName "Data.Binary.Put")
                              , impDecl (ModuleName "Data.Binary.Get")
                              ]
                  let decls = [dd, ib]
                  let mn    = ModuleName $ "Typeable.T"++(show $ identifier x)
                  return $ Module
                             sl
                             mn
                             [OptionsPragma sl Nothing "-XEmptyDataDecls"]                              -- [OptionPragma]
                             Nothing                         -- Maybe WarningText
                             Nothing                         -- Maybe [ExportSpec]
                             (nub $ ((concatMap imports [dd]) ++ imps) \\ [impDecl mn]) -- [ImportDecl]
                             decls

variables :: (PeanoNumber a) => Binding a Kind' b -> [TyVarBind]
variables  = f 0
             where
               f                 :: (PeanoNumber a) => Int -> Binding a Kind' b -> [TyVarBind]
               f n (Bind y x)     = (KindedVar (Ident [toEnum ((fromEnum 'a') + n)]) (k y)):(f (n+1) x)
               f _ (Expression _) = []
               k Concrete'          = KindStar
               k (Application' x y) = KindFn (k x) (k y)

dataDecl                   :: Definition Type' -> Context Decl
dataDecl t                  = do let typeName     = Ident $ show $ name t :: Name
                                 cs <- dataConstructors (structure t)
                                 return $ DataDecl 
                                            sl 
                                            Syntax.DataType
                                            [] 
                                            typeName
                                            (variables $ structure t)
                                            cs
                                            [
                                              (UnQual $ Ident "Eq",   [])
                                            , (UnQual $ Ident "Ord",  [])
                                            , (UnQual $ Ident "Show", [])
                                            , (UnQual $ Ident "Read", [])
                                            ]

dataConstructors                   :: (PeanoNumber a) => Binding a Kind' Type' -> Context [QualConDecl]
dataConstructors (Bind _ x)         = dataConstructors x
dataConstructors (Expression x)     = case constructors x of
                                        Nothing -> error "cannot haskellize abstract type"
                                        Just cs -> mapM f cs
                                      where
                                        f  :: (PeanoNumber a) => Constructor a -> Context QualConDecl
                                        f c = do fs <- mapM dataField (constructorFields c)
                                                 return $ QualConDecl
                                                            sl
                                                            []  -- [TyVarBind]
                                                            []  -- Context
                                                            $ RecDecl
                                                                (Ident $ show $ constructorName c)
                                                                fs  -- [([Name], BangType)]

dataField              :: (PeanoNumber a) => Field a -> Context ([Name], BangType)
dataField x             = do t <- dataType (fieldType x) 
                             return ([Ident $ show $ fieldName x], UnBangedTy t)

dataType                  :: (PeanoNumber a) => Type a -> Context Syntax.Type
dataType (DataType u)      = do n <- humanify u
                                return $ TyCon $ Qual (ModuleName $ "Typeable.T" ++ (show u)) (Ident n)
dataType (Variable v)      = return $ TyVar $ Ident [toEnum $ (fromEnum 'a') + (fromEnum v)] 
dataType (Application f a) = do x <- dataType f
                                y <- dataType a
                                return $ TyApp x y
dataType (Forall c)        = error "forall not implemented"

instanceBinary (DataDecl _ _ _ n vs cs _) 
                       = do let vs'  = map (\(KindedVar n _) -> TyVar n)  vs 
                            let cxt  = []
                            let fieldTypes = let u (QualConDecl _ _ _ (RecDecl _ xs)) = map (\(_, UnBangedTy t)->t) xs
                                             in  nub $ concatMap u cs :: [Syntax.Type]
                            let u (i,(QualConDecl _ _ _ (RecDecl n xs))) =
                                  InsDecl $ FunBind $ return $ Match 
                                    sl 
                                    (Ident "put") 
                                    [PApp (UnQual n) (map PVar fis)]
                                    Nothing
                                    (UnGuardedRhs e) 
                                    (BDecls [])
                                  where
                                   fis = map (Ident . return) $ zipWith const ['a'..] xs
                                   zs  = map (\x->Qualifier $ App (Var (UnQual $ Ident "put")) (Var $ UnQual x)) fis
                                   e   | length cs <= 1     = Do zs
                                       | length cs <= 255   = Do $ (Qualifier (App (Var (UnQual $ Ident "putWord8"))    (Lit $ Int i))):zs
                                       | length cs <= 65535 = Do $ (Qualifier (App (Var (UnQual $ Ident "putWord16be")) (Lit $ Int i))):zs
                                       | otherwise          = error "instanceBinary: too many constructors"
                            let get = InsDecl $ FunBind $ return $ Match 
                                      sl 
                                      (Ident "get") 
                                      []
                                      Nothing
                                      (UnGuardedRhs e) 
                                      (BDecls [])
                                  where
                                   e   | length cs == 0     = Var $ UnQual $ Ident "undefined"
                                       | length cs == 1     = Do $ (Generator sl (PVar $ Ident "index") $ App (Var $ UnQual $ Ident "return") (Lit $ Int 0)):zs
                                       | length cs <= 255   = Do $ (Generator sl (PVar $ Ident "index") $      Var $ UnQual $ Ident "getWord8")             :zs
                                       | length cs <= 65535 = Do $ (Generator sl (PVar $ Ident "index") $      Var $ UnQual $ Ident "getWord16")            :zs
                                       | otherwise          = error "instanceBinary: too many constructors"
                                   f i = Alt sl (PLit $ Int $ fromIntegral i) (UnGuardedAlt $ g (cs !! i)) (BDecls [])
                                   g (QualConDecl _ _ _ (RecDecl n xs)) 
                                       = let ns = zipWith (\x _->Ident $ 'a':(show x)) [0..] xs
                                         in  h ns $ App 
                                               (Var $ UnQual $ Ident "return") 
                                               $ foldl App (Con $ UnQual n) (map (Var . UnQual) ns)
                                   h [] x = x
                                   h (n:ns) x  = App
                                                  (App (Var $ UnQual $ Symbol ">>=") (Var $ UnQual $ Ident "get"))
                                                  (Lambda sl [PVar n] $ h ns x)  
                                   zs  = return $ Qualifier $ Case 
                                           (Var $ UnQual $ Ident "index")
                                           (map f [0..((length cs)-1)])
                            let puts | null cs   = return $ InsDecl $ FunBind $ return $ Match sl (Ident "put") [] Nothing (UnGuardedRhs $ Var $ UnQual $ Ident "undefined") (BDecls [])
                                     | otherwise = map u $ zip [0..] cs
                            return $ InstDecl 
                                       sl 
                                       (map (\t-> ClassA (Qual (ModuleName "Data.Binary") (Ident "Binary")) [t]) fieldTypes)
                                       (Qual (ModuleName "Data.Binary") (Ident "Binary"))
                                       [foldl TyApp (TyCon $ UnQual n) vs']
                                       (get:puts)
