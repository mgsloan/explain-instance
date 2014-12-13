{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module ExplainInstances where

import           Control.Applicative ((<$>))
import           Control.Monad (filterM)
import           Data.Generics
import qualified Data.Map as M
import           Data.Maybe
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany (reifyMany)

-- TODO:
--
-- * Handle ConstraintKinds
--
-- * Show info about type families by using Typeable

data Inst = Inst String [Inst]
    deriving (Show)

instanceResolvers :: [Name] -> Q [Dec]
instanceResolvers initial = do
    infos <- reifyMany recurse initial
    methodMap <- M.fromList <$> sequence
        [ (n, ) <$> chooseUnusedName ("resolve" ++ nameBase n)
        | (n, ClassI {}) <- infos
        ]
    renameMap <- M.fromList <$> sequence
        [ (n, ) <$> chooseUnusedName (nameBase n)
        | (n, _) <- infos
        ]
    return $ map (applySubstitution (`M.lookup` renameMap) . resolver methodMap) $
        concatMap (infoToDecs . snd) infos
  where
    recurse :: (Name, Info) -> Q (Bool, [Name])
    recurse (name, info) = return $ do
        case info of
            ClassI (ClassD cxt _name tvs _fds decs) insts ->
                (True, allNames (cxt, filter isFamilyDec decs) ++
                       concatMap tvKindNames tvs ++
                       concatMap instNames insts)
            TyConI (TySynD _name tvs ty) ->
                (True, allNames ty ++ concatMap tvKindNames tvs)
            -- Only need to clone data declarations when they have
            -- datatype contexts.
            TyConI (DataD cxt _name _tvs _cons _deriving) ->
                (not (null cxt), allNames cxt)
            TyConI (NewtypeD cxt _name _tvs _con _deriving) ->
                (not (null cxt), allNames cxt)
            -- We might encounter this due to DataKinds.
            DataConI _name _ty typeName _fixity ->
                (False, [typeName])
            -- FamilyI dec insts -> return (True, [])
            _ -> (False, [])
    instNames :: Dec -> [Name]
    instNames (InstanceD cxt ty decs) =
        allNames cxt ++ allNames ty ++ allNames (filter isFamilyDec decs)
    instNames _ = []
    lookupMethod :: M.Map Name Name -> Name -> Name
    lookupMethod methodMap name =
        fromMaybe (error ("Couldn't find method name for " ++ show name))
                  (M.lookup name methodMap)
    resolver :: M.Map Name Name -> Dec -> Dec
    resolver methodMap (ClassD cxt name tvs fds decs) =
        ClassD cxt name tvs fds $
            filter isFamilyDec decs ++
            [SigD method ty]
      where
        method = lookupMethod methodMap name
        ty = funT $
            map (AppT (ConT ''Proxy) . VarT . tvName) tvs ++
            [ConT ''Inst]
    resolver methodMap (InstanceD cxt ty decs) =
        InstanceD cxt ty $
            filter isFamilyDec decs ++
            [FunD method [Clause (map (\_ -> WildP) tvs) (NormalB expr) []]]
      where
        (ConT clazzName : tvs) = unAppsT ty
        method = lookupMethod methodMap clazzName
        expr = appsE'
            [ConE 'Inst, instHead, ListE (mapMaybe recursor cxt)]
        -- FIXME: add the substituted type info back in
        -- (use freeVarsT to generate Typeable instances for the free vars)
        -- instHead = AppE (VarE 'typeRep) (proxyE ty)
        instHead = LitE $ StringL $ pprint (InstanceD cxt ty [])
        recursor (ClassP n tys) = Just $ appsE' $
            VarE (lookupMethod methodMap n) : map proxyE tys
        recursor EqualP {} = Nothing
    resolver _ dec = dec

allNames :: Data a => a -> [Name]
allNames = listify (\_ -> True)

tvKindNames :: TyVarBndr -> [Name]
tvKindNames (KindedTV _name kind) = allNames kind
tvKindNames PlainTV {} = []

tvName :: TyVarBndr -> Name
tvName (KindedTV name _kind) = name
tvName (PlainTV name) = name

isFamilyDec FamilyD {} = True
isFamilyDec DataInstD {} = True
isFamilyDec NewtypeInstD {} = True
isFamilyDec TySynInstD {} = True
isFamilyDec ClosedTypeFamilyD {} = True
isFamilyDec _ = False

infoToDecs :: Info -> [Dec]
infoToDecs (ClassI dec insts) = dec : insts
infoToDecs (TyConI dec) = [dec]
infoToDecs (FamilyI dec insts) = dec : insts
infoToDecs (VarI _name _ty mdec _fixity) = maybeToList mdec
infoToDecs ClassOpI {} = []
infoToDecs PrimTyConI {} = []
infoToDecs DataConI {} = []
infoToDecs TyVarI {} = []

chooseUnusedName :: String -> Q Name
chooseUnusedName name = do
    -- This will always match, since there can only be finite names.
    Just str <- findM (\str -> not <$> exists str) choices
    return (mkName str)
  where
    choices = map (name ++) $ "" : "_" : map ('_':) (map show [0..])
    exists str = do
        mtype <- lookupTypeName str
        mvalue <- lookupValueName str
        return $ isJust mtype || isJust mvalue

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f (x:xs) = do
    b <- f x
    if b
        then return (Just x)
        else findM f xs

applySubstitution :: (Data a, Typeable b) => (b -> Maybe b) -> a -> a
applySubstitution f = everywhere (id `extT` (\x -> fromMaybe x (f x)))

funT :: [Type] -> Type
funT [x] = x
funT (x:xs) = AppT (AppT ArrowT x) (funT xs)

unAppsT :: Type -> [Type]
unAppsT ty = go ty []
  where
    go (AppT l r) = go l . (r :)
    go ty = (ty :)

appsE' :: [Exp] -> Exp
appsE' xs = go (reverse xs)
  where
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
