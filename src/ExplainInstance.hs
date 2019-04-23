{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module ExplainInstance where

#if !(MIN_VERSION_template_haskell(2,10,0))
import           Control.Applicative ((<$>), (<*>))
#else
import           Control.Applicative ((<$>))
#endif
import           Data.Char (isLower)
import           Data.Function (on)
import           Data.Generics
import           Data.List (groupBy, sortBy, sort, group, find)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ord (comparing)
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany (reifyMany)

-- TODO:
--
-- * Show info about type families by using Typeable
--
-- * Use a parser so that type vars can be provided. Or, have a
-- wildcard datatype.
--
-- * Apply inverse renaming on output types (relevant to constraints
-- that end up in the types, as well as data types with contexts)

-- Issues due to TH limitations:
--
-- * No ConstraintKinds
--
-- * No PolyKinds
--
-- * No info about whether an instance is overlapping / incoherent
--
-- * No direct GHCI support. This is because 'explainInstances' can't
-- yield an Exp, as 'addTopDecl' cannot create instances / typeclasses.

explainInstance :: Q Type -> Q [Dec]
explainInstance = explainInstance' False

explainInstanceError :: Q Type -> Q [Dec]
explainInstanceError = explainInstance' True

explainInstance' :: Bool -> Q Type -> Q [Dec]
explainInstance' addErrorInstance qty = do
    ty <- qty
    case unAppsT ty of
        (ConT clazz : tys) -> do
            (decs, methodMap, renameMap) <- instanceResolvers addErrorInstance [clazz]
            let tys' = applySubstMap renameMap tys
            decs' <- [d| main = putStrLn (displayInst $(return (invokeResolve methodMap clazz tys'))) |]
            return (decs' ++ decs)
        _ -> fail "explainInstance input should be a constraint"

-- | An explanation of why some constraint is satisfied.
data Inst = Inst
    { -- | Text of an instance declaration, but without the method declarations.
      instHead :: String
      -- | Describes how type variables are instantiated in the head.
    , instTypes :: [(String, TypeRep)]
      -- | Explanations of how the instance's constraints are satisfied.
    , instConstraints :: [Inst]
    }
    deriving (Show)

-- | Provides an indented string presentation of the instance resolution tree.
displayInst :: Inst -> String
displayInst = go 0
  where
    go i (Inst decl vars ctx) =
        addIndent i (decl ++ displayVars vars) ++
        concatMap (("\n" ++) . go (i + 2)) ctx
    displayVars [] = ""
    displayVars (var0 : vars) =
        "\n  with " ++ displayVar var0 ++
        "\n" ++ unlines (map (("       " ++) . displayVar) vars)
    displayVar (n, ty) = n ++ " ~ " ++ showsPrec 9 ty ""

instanceResolvers :: Bool -> [Name] -> Q ([Dec], M.Map Name Name, M.Map Name Name)
instanceResolvers addErrorInstance initial = do
    infos <- reifyMany recurse initial
    methodMap <- M.fromList <$> sequence
        [ (n, ) <$> chooseUnusedName True ("resolve" ++ alphaName n)
        | (n, ClassI {}) <- infos
        ]
    let names = map fst infos ++ concatMap (map conName . infoCons . snd) infos
    renameMap <- M.fromList <$>
        mapM (\n -> (n, ) <$> chooseUnusedName False (alphaName n)) names
    decs <- mapM (resolver methodMap) (concatMap (infoToDecs . snd) infos)
    return (map (applySubst (flip M.lookup renameMap)) decs, methodMap, renameMap)
  where
    -- Recursively enumerate all of the top level declarations which
    -- need to be copied / renamed.
    recurse :: (Name, Info) -> Q (Bool, [Name])
    recurse (_, info) = do
        let (shouldEmit, names) = case info of
                ClassI (ClassD ctx _name tvs _fds decs) insts ->
                    (True, allNames (ctx, filter isFamilyDec decs) ++
                           concatMap tvKindNames tvs ++
                           concatMap instNames insts)
                TyConI (TySynD _name tvs ty) ->
                    (True, allNames ty ++ concatMap tvKindNames tvs)
                -- Only need to clone data declarations when they have
                -- datatype contexts.
#if MIN_VERSION_template_haskell(2,11,0)
                TyConI (DataD ctx _name _tvs _kind _cons _deriving) ->
#else
                TyConI (DataD ctx _name _tvs _cons _deriving) ->
#endif
                    (not (null ctx), allNames ctx)
#if MIN_VERSION_template_haskell(2,11,0)
                TyConI (NewtypeD ctx _name _tvs _kind _con _deriving) ->
#else
                TyConI (NewtypeD ctx _name _tvs _con _deriving) ->
#endif
                    (not (null ctx), allNames ctx)
            -- We might encounter this due to DataKinds.
#if MIN_VERSION_template_haskell(2,11,0)
                DataConI _name _ty typeName ->
#else
                DataConI _name _ty typeName _fixity ->
#endif
                    (False, [typeName])
            -- FamilyI dec insts -> return (True, [])
                _ -> (False, [])
            filteredNames = filter (not . isLower . head . nameBase) names
        return (shouldEmit, filteredNames)
    instNames :: Dec -> [Name]
#if MIN_VERSION_template_haskell(2,11,0)
    instNames (InstanceD _ ctx ty decs) =
#else
    instNames (InstanceD ctx ty decs) =
#endif
        allNames ctx ++ allNames ty ++ allNames (filter isFamilyDec decs)
    instNames _ = []
    infoToDecs :: Info -> [Dec]
    -- TODO: check fundeps?
    infoToDecs (ClassI dec@(ClassD _ name tvs _ _) insts) =
        case addErrorInstance && not hasDefaultCase of
            False -> dec : insts
            True ->
                let errInst = InstanceD
#if MIN_VERSION_template_haskell(2,11,0)
                        Nothing
#endif
                        []
                        (appsT $ ConT name : map (VarT . tvName) tvs)
                        errorInstanceDecs
                in dec : errInst : insts
      where
        -- If true then an overlapping instance like (Class v1 ..),
        -- where all arguments are type variables, already exists.
        -- In this case omit the error instance.
        hasDefaultCase = isJust $ find isDefaultCase insts
#if MIN_VERSION_template_haskell(2,11,0)
        isDefaultCase (InstanceD _ _ (unAppsT -> (_:tys)) _) =
#else
        isDefaultCase (InstanceD _ (unAppsT -> (_:tys)) _) =
#endif
            all isTyVar tys
        isDefaultCase _ = False
        isTyVar (VarT _) = True
        isTyVar _ = False
    infoToDecs (ClassI _ _) = error "impossible: ClassI which doesn't contain ClassD"
    infoToDecs (TyConI dec) = [dec]
    infoToDecs (FamilyI dec insts) = dec : insts
#if MIN_VERSION_template_haskell(2,11,0)
    infoToDecs (VarI _name _ty mdec) = maybeToList mdec
#else
    infoToDecs (VarI _name _ty mdec _fixity) = maybeToList mdec
#endif
    infoToDecs ClassOpI {} = []
    infoToDecs PrimTyConI {} = []
    infoToDecs DataConI {} = []
    infoToDecs TyVarI {} = []
    errorInstanceDecs = [FunD (mkName "x") []]
    -- Modify a class or instance to instead just have a single
    -- "resolver*" function.
    resolver :: M.Map Name Name -> Dec -> Q Dec
    resolver methodMap (ClassD ctx' name tvs fds decs) = do
        let method = lookupMethod methodMap name
            ty = funT $
                map (AppT (ConT ''Proxy) . VarT . tvName) tvs ++
                [ConT ''Inst]
        ctx <- mapM trimConstraint ctx'
        return $ ClassD ctx name tvs fds $
            filter isFamilyDec decs ++
            [SigD method ty]
#if MIN_VERSION_template_haskell(2,11,0)
    resolver methodMap (InstanceD overlap ctx' instTy' decs) = do
#else
    resolver methodMap (InstanceD ctx' instTy' decs) = do
#endif
        ctx <- mapM trimConstraint ctx'
        instTy <- trimInstanceType instTy'
        let substs = varTSubsts (ctx, instTy)
            cleanTyVars = applySubstMap (M.fromList substs)
#if MIN_VERSION_template_haskell(2,11,0)
        cleanedHead <- cleanTyCons $ cleanTyVars $ InstanceD overlap ctx instTy []
#else
        cleanedHead <- cleanTyCons $ cleanTyVars $ InstanceD ctx instTy []
#endif
        let (ConT clazzName : tvs) = unAppsT instTy
            method = lookupMethod methodMap clazzName
            msg = case addErrorInstance of
                True | decs == errorInstanceDecs -> "ERROR " ++ pprint cleanedHead
                _ -> pprint cleanedHead
            expr = appsE'
                [ ConE 'Inst
                , LitE $ StringL msg
                , ListE $ flip map substs $ \(ty, cty) -> TupE
                    [ LitE (StringL (pprint cty))
                    , AppE (VarE 'typeRep) (proxyE (VarT ty))
                    ]
#if MIN_VERSION_template_haskell(2,10,0)
                , ListE $ flip mapMaybe ctx $ \ctxTy -> case unAppsT ctxTy of
                    EqualityT : _ -> Nothing
                    ConT n : tys -> Just (invokeResolve methodMap n tys)
                    _ -> Nothing
                ]
            -- Need extra typeable constraints in order to use typeRep.
            extraCtx = map (\(n, _) -> appsT [ConT ''Typeable, VarT n]) substs
#else
                , ListE $ flip mapMaybe ctx $ \case
                    EqualP {} -> Nothing
                    ClassP n tys -> Just (invokeResolve methodMap n tys)
                ]
            -- Need extra typeable constraints in order to use typeRep.
            extraCtx = map (ClassP ''Typeable . (: []) . VarT . fst) substs
#endif
#if MIN_VERSION_template_haskell(2,11,0)
        return $ InstanceD overlap (ctx ++ extraCtx) instTy $
#else
        return $ InstanceD (ctx ++ extraCtx) instTy $
#endif
            filter isFamilyDec decs ++
            [FunD method [Clause (map (\_ -> WildP) tvs) (NormalB expr) []]]
    resolver _ dec = return dec

invokeResolve :: M.Map Name Name -> Name -> [Type] -> Exp
invokeResolve methodMap name tys =
    appsE' $ VarE (lookupMethod methodMap name) : map proxyE tys

lookupMethod :: M.Map Name Name -> Name -> Name
lookupMethod methodMap name =
    fromMaybe (error ("Couldn't find resolve* method name for " ++ show name))
              (M.lookup name methodMap)

allNames :: Data a => a -> [Name]
allNames = listify (\_ -> True)

tvKindNames :: TyVarBndr -> [Name]
tvKindNames (KindedTV _name kind) = allNames kind
tvKindNames PlainTV {} = []

tvName :: TyVarBndr -> Name
tvName (KindedTV name _kind) = name
tvName (PlainTV name) = name

isFamilyDec :: Dec -> Bool
#if MIN_VERSION_template_haskell(2,11,0)
isFamilyDec OpenTypeFamilyD {} = True
#else
isFamilyDec FamilyD {} = True
#endif
isFamilyDec DataInstD {} = True
isFamilyDec NewtypeInstD {} = True
isFamilyDec TySynInstD {} = True
isFamilyDec ClosedTypeFamilyD {} = True
isFamilyDec _ = False

-- Work around a TH bug where PolyKinded constraints get too many
-- arguments.
#if !(MIN_VERSION_template_haskell(2,10,0))
trimConstraint :: Pred -> Q Pred
trimConstraint (ClassP n tys) = do
    info <- reify n
    case info of
        ClassI (ClassD _ _ tvs _ _) _ ->
            return $ ClassP n (drop (length tys - length tvs) tys)
        _ -> fail $ unwords
            [ "Expected to reify a class but for"
            , pprint n
            , "instead got\n"
            , pprint info
            ]
trimConstraint x = return x
#else
trimConstraint :: Type -> Q Type
trimConstraint (unAppsT -> (ConT n : tys)) = do
    info <- reify n
    case info of
        ClassI (ClassD _ _ tvs _ _) _ ->
            return $ appsT (ConT n : drop (length tys - length tvs) tys)
        _ -> fail $ unwords
            [ "Expected to reify a class but for"
            , pprint n
            , "instead got\n"
            , pprint info
            ]
trimConstraint x = return x
#endif

trimInstanceType :: Type -> Q Type
trimInstanceType (unAppsT -> (ConT n : tys)) = do
    ClassI (ClassD _ _ tvs _ _) _ <- reify n
    return $ appsT (ConT n : (drop (length tys - length tvs) tys))
trimInstanceType _ = fail "Expected instance type to start with a typeclass name"

chooseUnusedName :: Bool -> String -> Q Name
chooseUnusedName allowUnmodified name = do
    -- This will always match, since there can only be finite names.
    Just str <- findM (\str -> not <$> exists str) choices
    return (mkName str)
  where
    choices = map (name ++) $
        (if allowUnmodified then ("" : ) else id) $
        "_" : map ('_':) (map show ([1..] :: [Int]))
    -- 'recover' is used to handle errors due to ambiguous identifier names.
    exists str = recover (return True) $ do
        mtype <- lookupTypeName str
        mvalue <- lookupValueName str
        return $ isJust mtype || isJust mvalue

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = do
    b <- f x
    if b
        then return (Just x)
        else findM f xs

applySubst :: (Data a, Typeable b) => (b -> Maybe b) -> a -> a
applySubst f = everywhere (id `extT` (\x -> fromMaybe x (f x)))

applySubstMap :: (Data a, Ord b, Typeable b) => M.Map b b -> a -> a
applySubstMap m = applySubst (flip M.lookup m)

funT :: [Type] -> Type
funT [] = error "Invariant violated: funT invoked with empty list"
funT [x] = x
funT (x:xs) = AppT (AppT ArrowT x) (funT xs)

appsT :: [Type] -> Type
appsT = go . reverse
  where
    go [] = error "Invariant violated: appsT invoked with empty list"
    go [x] = x
    go (x:xs) = AppT (go xs) x

unAppsT :: Type -> [Type]
unAppsT ty = go ty []
  where
    go (AppT l r) = go l . (r :)
    go t = (t :)

appsE' :: [Exp] -> Exp
appsE' = go . reverse
  where
    go [] = error "Invariant violated: appsE' invoked with empty list"
    go [x] = x
    go (x:xs) = AppE (go xs) x

proxyE :: Type -> Exp
proxyE = SigE (ConE 'Proxy) . AppT (ConT ''Proxy)

freeVarsT :: Type -> [Name]
freeVarsT (ForallT tvs _ ty) = filter (`notElem` (map tvName tvs)) (freeVarsT ty)
freeVarsT (AppT l r) = freeVarsT l ++ freeVarsT r
freeVarsT (SigT ty k) = freeVarsT ty ++ freeVarsT k
freeVarsT (VarT n) = [n]
freeVarsT _ = []

-- Dequalify names which are unambiguous.
cleanTyCons :: Data a => a -> Q a
cleanTyCons = everywhereM $
    return
    `extM` subst1
#if !(MIN_VERSION_template_haskell(2,10,0))
    `extM` subst2
#endif
  where
    rename :: Name -> Q Name
    rename n = do
        inScope <- typeNameInScope n
        return $ if inScope then mkName (nameBase n) else n
    subst1 (ConT n) = ConT <$> rename n
    subst1 x = return x
#if !(MIN_VERSION_template_haskell(2,10,0))
    subst2 (ClassP n tys) = ClassP <$> rename n <*> return tys
    subst2 x = return x
#endif

typeNameInScope :: Name -> Q Bool
typeNameInScope n =
    recover (return False)
            ((Just n ==) <$> lookupTypeName (nameBase n))

-- Chooses prettier names for type variables.  Assumes that all type
-- variable names are unique.
varTSubsts :: Data a => a -> [(Name, Name)]
varTSubsts =
    concatMap munge . groupSortOn nameBase . sortNub . varTNames
  where
    munge = zipWith (\s x -> (x, mkName (nameBase x ++ s))) ("" : map show ([1..] :: [Int]))

varTNames :: Data a => a -> [Name]
varTNames x = [n | VarT n <- listify (\_ -> True) x]

infoCons :: Info -> [Con]
#if MIN_VERSION_template_haskell(2,11,0)
infoCons (TyConI (DataD _ _ _ _ cons _)) = cons
infoCons (TyConI (NewtypeD _ _ _ _ con _)) = [con]
#else
infoCons (TyConI (DataD _ _ _ cons _)) = cons
infoCons (TyConI (NewtypeD _ _ _ con _)) = [con]
#endif
infoCons _ = []

conName :: Con -> Name
conName (NormalC name _) = name
conName (RecC name _) = name
conName (InfixC _ name _) = name
conName (ForallC _ _ con) = conName con

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = groupBy ((==) `on` f) . sortBy (comparing f)

sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort

addIndent :: Int -> String -> String
addIndent cnt = unlines . map (replicate cnt ' ' ++) . lines

alphaName :: Name -> String
alphaName n
    | nameBase n == "~" = "Tilde"
    | otherwise = nameBase n
