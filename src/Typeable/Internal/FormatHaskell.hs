{-# OPTIONS -XFlexibleContexts -XScopedTypeVariables #-}
module Typeable.Internal.FormatHaskell where

import Typeable.T9e2e1e478e094a8abe5507f8574ac91f --Succ
import Typeable.T421496848904471ea3197f25e2a02b72 --Zero
import Typeable.T0174bd2264004820bfe34e211cb35a7d --DataType
import Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 --Definition
import qualified Typeable.T205895c8d2df475b8d5ead5ee33d9f63 as Field 
import qualified Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3 as Constructor 
import qualified Typeable.T3e81531118e14888be21de7921b15bb5 as Type 
import qualified Typeable.T1660b01f08dc4aedbe4c0941584541cb as K --Kind

import Data.List
import Data.String
import Data.Char
import Data.UUID hiding (null)
import Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S

import Language.Haskell.Exts.Syntax           hiding (Context, Type, DataType)
import qualified Language.Haskell.Exts.Syntax as     Syntax

import Typeable.Internal.Context
import Typeable.Internal.Misc
import Typeable.Internal.Graph

sl = SrcLoc "" 0 0

importMapping  :: (Monad m) => Context m (M.Map UUID (S.Set UUID))
importMapping   = getTypes >>= return . M.map imports 
                  where
                    imports                       :: Definition Type.Type -> S.Set UUID
                    imports                        = imports' . structure
                    imports'                      :: (PeanoNumber a) => Type.Type a -> S.Set UUID
                    imports' (Type.Quantification _ x)  = imports'  x
                    imports' (Type.Type _ x)       = case x of
                                                       Nothing -> S.empty
                                                       Just cs -> foldr (S.union . imports''') S.empty cs
                    imports'''                    :: (PeanoNumber a) => Constructor.Constructor a -> S.Set UUID
                    imports'''                     = (foldr (S.union . imports'''') S.empty) . Constructor.fields
                    imports''''                   :: (PeanoNumber a) => Field.Field a -> S.Set UUID
                    imports''''                    = imports''''' . Field.type_
                    imports'''''                  :: (PeanoNumber a) => DataType a -> S.Set UUID
                    imports''''' (DataType u)      = S.singleton u
                    imports''''' (Application a b) = imports''''' a `S.union` imports''''' b
                    imports'''''  _                = S.empty

constructorCount :: Definition Type.Type -> Maybe Int
constructorCount  = f . structure
                    where
                      f :: (PeanoNumber a) => Type.Type a -> Maybe Int
                      f (Type.Quantification _ x) = f x
                      f (Type.Type _ x)      = case x of
                                                 Nothing -> Nothing
                                                 Just cs -> Just $ length cs

nonNullaryConstructors :: Definition Type.Type -> Bool
nonNullaryConstructors  = f . structure
                    where
                      f :: (PeanoNumber a) => Type.Type a -> Bool
                      f (Type.Quantification _ x) = f x
                      f (Type.Type _ x)      = case x of
                                                 Nothing -> False
                                                 Just cs -> or $ map g cs
                      g :: (PeanoNumber a) => Constructor.Constructor a -> Bool
                      g = not . null . Constructor.fields

typeModule  :: (Monad m) => Bool -> Definition Type.Type -> Context m Module
typeModule b x 
             = do dd      <- dataDecl b x
                  iEq     <- instanceOf (Qual (ModuleName "Prelude") (Ident "Eq"))   [Symbol "=="]                                                   b dd
                  iOrd    <- instanceOf (Qual (ModuleName "Prelude") (Ident "Ord"))  [Ident "compare"]                                               b dd
                  iEnum   <- instanceOf (Qual (ModuleName "Prelude") (Ident "Enum")) [Ident "succ", Ident "pred", Ident "toEnum", Ident "fromEnum"]  b dd
                  iShow   <- instanceOf (Qual (ModuleName "Prelude") (Ident "Show")) [Ident "show"]                                                  b dd
                  iRead   <- instanceOf (Qual (ModuleName "Prelude") (Ident "Read")) [Ident "readsPrec"]  b dd
                  iTypeable  <- instanceTypeable  b x dd
                  --iData      <- instanceData      b x dd
                  iTypeIdent <- instanceTypeIdent b x dd
                  iBinary    <- instanceBinary    b x dd
                  im <- importMapping
                  let impDecl m = ImportDecl sl m True False Nothing Nothing Nothing
                  let imps2  = [ let s = identifier x `S.member` runGraph (successorsToDepth 5 z) im
                                 in  ImportDecl sl (ModuleName $ "Typeable.T"++(show' z)) True s Nothing Nothing Nothing   
                               | z <- S.toList $ M.findWithDefault S.empty (identifier x) im 
                               , identifier x /= z
                               , not b
                               ]  
                  let imps1  = [ 
                                (impDecl (ModuleName "Prelude")) { importQualified = False
                                                                 , importSpecs = Just (False, [ IVar (Ident  "fromInteger")
                                                                                              , IVar (Ident  "return")
                                                                                              , IVar (Ident  "fail")
                                                                                              , IVar (Ident  "undefined")
                                                                                              , IVar (Symbol ">>=")
                                                                                              , IVar (Symbol ">>")
                                                                                              , IVar (Symbol "==")
                                                                                              ])
                                                                 }
                              , impDecl (ModuleName "Prelude")
                              , impDecl (ModuleName "Data.Tree")
                              , impDecl (ModuleName "Data.Data")
                              , impDecl (ModuleName "Data.Typeable")
                              , impDecl (ModuleName "Data.Typeable.Extra")
                              , impDecl (ModuleName "Data.Binary")
                              , impDecl (ModuleName "Data.Binary.Put")
                              , impDecl (ModuleName "Data.Binary.Get")
                              , impDecl (ModuleName "Data.EBF")
                              , (impDecl $ ModuleName "Data.String") { importQualified = False }
                              ] 
                  let decls = [dd, iEq, iOrd, iShow, iBinary, iTypeable, iTypeIdent] ++[iEnum | not $ nonNullaryConstructors x, null $ variables $ structure x]
                  let mn    = ModuleName $ "Typeable.T"++(show' $ identifier x)
                  return $ Module
                             sl
                             mn
                             [
                               OptionsPragma sl Nothing "-XEmptyDataDecls"
                             , OptionsPragma sl Nothing "-XKindSignatures"
                             , OptionsPragma sl Nothing "-XNoImplicitPrelude"
                             , OptionsPragma sl Nothing "-XFlexibleContexts"
                             , OptionsPragma sl Nothing "-XUndecidableInstances"
                             , OptionsPragma sl Nothing "-XStandaloneDeriving"
                             , OptionsPragma sl Nothing "-XOverloadedStrings"
                             , OptionsPragma sl Nothing "-XDeriveDataTypeable"
                             ]                           -- [OptionPragma]
                             Nothing                     -- Maybe WarningText
                             Nothing                     -- Maybe [ExportSpec]
                             (nub $ imps1 ++ imps2)      -- [ImportDecl]
                             decls

variables :: (PeanoNumber a) => Type.Type a -> [TyVarBind]
variables  = f 0
             where
               f                 :: (PeanoNumber a) => Int -> Type.Type a -> [TyVarBind]
               f n (Type.Quantification y x)  = (KindedVar (Ident [toEnum ((fromEnum 'a') + n)]) (k y)):(f (n+1) x)
               f _ (Type.Type _ _)            = []
               k K.KindStar              = KindStar
               k (K.KindApplication x y) = KindFn (k x) (k y)

dataDecl                   :: (Monad m) => Bool -> Definition Type.Type -> Context m Decl
dataDecl b t                = do let typeName     = Ident $ show' $ name t :: Name
                                 cs <- dataConstructors (structure t)
                                 return $ DataDecl 
                                            sl 
                                            Syntax.DataType
                                            [] 
                                            typeName
                                            (variables $ structure t)
                                            (if not b then cs else [])
                                            []

dataConstructors                   :: (PeanoNumber a, Monad m) => Type.Type a -> Context m [QualConDecl]
dataConstructors (Type.Quantification _ x)  = dataConstructors x
dataConstructors (Type.Type s c)    = case c of
                                        Nothing -> fail "cannot haskellize abstract type"
                                        Just cs -> mapM f cs
                                      where
                                        f  :: (PeanoNumber a, Monad m) => Constructor.Constructor a -> Context m QualConDecl
                                        f c = do fs <- mapM dataField (Constructor.fields c)
                                                 return $ QualConDecl
                                                            sl
                                                            []  -- [TyVarBind]
                                                            []  -- Context
                                                            $ RecDecl
                                                                (Ident $ show' $ Constructor.name c)
                                                                fs  -- [([Name], BangType)]

dataField              :: (PeanoNumber a, Monad m) => Field.Field a -> Context m ([Name], BangType)
dataField x             = do t <- dataType (Field.type_ x) 
                             return ([Ident $ haskape $ g $ show' $ Field.name x], UnBangedTy t)
                          where
                            g (x:xs) = (toLower x):xs

haskape x | x `elem` ["case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in"
                     , "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where"
                     ] = x ++ "_"
          | otherwise  = x

dataType                  :: (PeanoNumber a, Monad m) => DataType a -> Context m Syntax.Type
dataType (DataType u)      = do n <- humanify u
                                return $ TyCon $ Qual (ModuleName $ "Typeable.T" ++ (show' u)) (Ident n)
dataType (Variable v)      = return $ TyVar $ Ident [toEnum $ (fromEnum 'a') + (fromEnum v)] 
dataType (Application f a) = do x <- dataType f
                                y <- dataType a
                                return $ TyApp x y
dataType (Forall _ _)      = fail "FormatHaskell.dataType: forall not implemented"

instanceOf cn cms b (DataDecl _ _ _ n vs cs _)
  = do let vs'  = map (\(KindedVar n _) -> TyVar n)  vs 
       let vA (TyApp a b) = vA a
           vA (TyCon _  ) = False
           vA (TyVar _  ) = True
           vA _           = False
       let starVariables = map (\(KindedVar n _) -> TyVar n) $ filter (\(KindedVar _ k) -> k == KindStar) vs
       let fieldTypes = let u (QualConDecl _ _ _ (RecDecl _ xs)) = map (\(_, UnBangedTy t)->t) xs
                        in  nub $ concatMap u cs :: [Syntax.Type]
       let ctxTypes   = nub $ (filter vA fieldTypes) ++ starVariables
       return $ if not $ null cs 
                  then DerivDecl
                         sl
                         (map (\t-> ClassA cn [t]) ctxTypes)
                         cn
                         [foldl TyApp (TyCon $ UnQual n) vs']
                  else InstDecl 
                         sl 
                         (map (\t-> ClassA cn [t]) ctxTypes)
                         cn
                         [foldl TyApp (TyCon $ UnQual n) vs']
                         (if b 
                            then [] 
                            else map (\cm-> InsDecl $ FunBind $ return $ Match sl cm [] Nothing (UnGuardedRhs $ Var $ UnQual $ Ident "undefined") (BDecls [])) cms
                         )

instanceTypeable b x (DataDecl _ _ _ n vs cs _) 
  = do let kindCount KindStar     = 0 :: Int
           kindCount (KindFn _ k) = 1+(kindCount k)
       let vcs   = map (\(KindedVar _ k)-> kindCount k) vs
       let dt  x = Qual (ModuleName "Data.Typeable") (Ident x)
       let dte x = Qual (ModuleName $ "Data.Typeable" ++ (if all (==0) vcs then "" else ".Extra")) (Ident x)
       let order | all (==0) vcs = if null vcs then "" else show $ length vcs 
                 | otherwise     = "_"++(concatMap show vcs)
       return $ InstDecl 
                  sl 
                  []
                  (dte $ "Typeable" ++ order)
                  [TyCon $ UnQual n]
                  (if b 
                    then [] 
                    else return $ InsDecl $ FunBind $ return $ Match
                           sl
                           (Ident $ "typeOf" ++ order)
                           [PWildCard]
                           Nothing
                           (UnGuardedRhs $
                             App
                               (App
                                 (Var (dt "mkTyConApp"))
                                 (App
                                   (Var (dt "mkTyCon"))
                                   (Lit $ String $ "Typeable.T"++(show' $ identifier x)++"."++(show' $ name x))
                                 )
                               )
                               (List []) 
                           )
                           (BDecls [])
                  )       

instanceData b x (DataDecl _ _ _ n vs cs _) 
  = do let vs'  = map (\(KindedVar n _) -> TyVar n)  vs 
       let vA (TyApp a b) = vA a
           vA (TyCon _  ) = False
           vA (TyVar _  ) = True
           vA _           = False
       let starVariables = map (\(KindedVar n _) -> TyVar n) $ filter (\(KindedVar _ k) -> k == KindStar) vs
       let fieldTypes = let u (QualConDecl _ _ _ (RecDecl _ xs)) = map (\(_, UnBangedTy t)->t) xs
                        in  nub $ concatMap u cs :: [Syntax.Type]
       let ctxTypes   = nub $ (filter vA fieldTypes) ++ starVariables
       let cn = Qual (ModuleName "Data.Data") (Ident "Data")
       return $ if not $ null cs 
                  then DerivDecl
                         sl
                         ((map (\t-> ClassA cn [t]) ctxTypes) ++ (map (\t-> ClassA (Qual (ModuleName "Prelude") (Ident "Ord")) [t]) ctxTypes))
                         cn
                         [foldl TyApp (TyCon $ UnQual n) vs']
                  else InstDecl 
                         sl 
                         ((map (\t-> ClassA cn [t]) ctxTypes) ++ (map (\t-> ClassA (Qual (ModuleName "Prelude") (Ident "Ord")) [t]) ctxTypes))
                         cn
                         [foldl TyApp (TyCon $ UnQual n) vs']
                         (if b 
                            then [] 
                            else map 
                                   (\cm-> InsDecl $ FunBind $ return $ Match sl (Ident cm) [] Nothing (UnGuardedRhs $ Var $ UnQual $ Ident "undefined") (BDecls [])) 
                                   ["gfoldl", "gunfold", "toConstr", "dataTypeOf", "dataCast1"]
                         )
 

instanceTypeIdent b x (DataDecl _ _ _ n vs cs _) 
  = do let k KindStar     = "S"
           k (KindFn a b) = "A"++(k a)++(k b)
       let order = concatMap (\(KindedVar _ y)-> k y) vs
       let dt  x = Qual (ModuleName "Data.EBF") (Ident x)
       return $ InstDecl 
                  sl 
                  []
                  (dt $ "TypeIdent" ++ order)
                  [TyCon $ UnQual n]
                  (if b 
                    then [] 
                    else return $ InsDecl $ FunBind $ return $ Match
                           sl
                           (Ident $ "typeOf" ++ order)
                           [PWildCard]
                           Nothing
                           (UnGuardedRhs $
                             App
                               (App
                                 (Var $ Qual (ModuleName "Data.Tree") (Ident "Node"))
                                 (Lit $ String $ Data.UUID.toString $ identifier x)
                               )
                               (List []) 
                           )
                           (BDecls [])
                  )       



instanceBinary b x (DataDecl _ _ _ n vs cs _) 
  = do let vs'  = map (\(KindedVar n _) -> TyVar n)  vs 
       let vA (TyApp a b) = vA a
           vA (TyCon _  ) = False
           vA (TyVar _  ) = True
           vA _           = False
       let starVariables = map (\(KindedVar n _) -> TyVar n) $ filter (\(KindedVar _ k) -> k == KindStar) vs
       let fieldTypes = let u (QualConDecl _ _ _ (RecDecl _ xs)) = map (\(_, UnBangedTy t)->t) xs
                        in  nub $ concatMap u cs :: [Syntax.Type]
       let ctxTypes   = nub $ (filter vA fieldTypes) ++ starVariables
 
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
              zs  = map (\x->Qualifier $ App (Var (Qual (ModuleName "Data.EBF") $ Ident "put")) (Var $ UnQual x)) fis
              e   | length cs <= 1     = if null zs 
                                           then App (Var (UnQual $ Ident "return")) (Tuple [])
                                           else Do zs
                  | length cs <= 255   = Do $ (Qualifier (App (Var (Qual (ModuleName "Data.Binary.Put") $ Ident "putWord8"))    (Lit $ Int i))):zs
                  | length cs <= 65535 = Do $ (Qualifier (App (Var (Qual (ModuleName "Data.Binary.Put") $ Ident "putWord16be")) (Lit $ Int i))):zs
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
                  | length cs <= 255   = Do $ (Generator sl (PVar $ Ident "index") $      Var $ Qual (ModuleName "Data.Binary.Get") $ Ident "getWord8"):zs
                  | length cs <= 65535 = Do $ (Generator sl (PVar $ Ident "index") $      Var $ Qual (ModuleName "Data.Binary.Get") $ Ident "getWord16be"):zs
                  | otherwise          = error "instanceBinary: too many constructors"
              f i = Alt sl (PLit $ Int $ fromIntegral i) (UnGuardedAlt $ g (cs !! i)) (BDecls [])
              g (QualConDecl _ _ _ (RecDecl n xs)) 
                  = let ns = zipWith (\x _->Ident $ 'a':(show x)) [0..] xs
                    in  h ns $ App 
                          (Var $ UnQual $ Ident "return") 
                          $ foldl App (Con $ UnQual n) (map (Var . UnQual) ns)
              h [] x = x
              h (n:ns) x  = App
                             (App (Var $ UnQual $ Symbol ">>=") (Var $ Qual (ModuleName "Data.EBF") $ Ident "get"))
                             (Lambda sl [PVar n] $ h ns x)  
              zs  = return $ Qualifier $ Case 
                      (Var $ UnQual $ Ident "index")
                      (map f [0..((length cs)-1)])
       let puts | null cs   = return $ InsDecl $ FunBind $ return $ Match sl (Ident "put") [] Nothing (UnGuardedRhs $ Var $ UnQual $ Ident "undefined") (BDecls [])
                | otherwise = map u $ zip [0..] cs

       let k KindStar     = "S"
           k (KindFn a b) = "A"++(k a)++(k b)
           k' KindStar    = ""
           k' (KindFn a _) = k a
       let idents         = map (\(KindedVar n y)-> ClassA (Qual (ModuleName "Data.EBF") (Ident $ "TypeIdent" ++ (k' y))) [TyVar n]) vs
       return $ InstDecl 
                  sl 
                  ((map (\t-> ClassA (Qual (ModuleName "Data.EBF") (Ident "EBF")) [t]) ctxTypes)++idents)
                  (Qual (ModuleName "Data.EBF") (Ident "EBF"))
                  [foldl TyApp (TyCon $ UnQual n) vs']
                  (if b then [] else get:puts)
