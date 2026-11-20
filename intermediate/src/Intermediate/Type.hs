module Intermediate.Type (TypeVar (..), ArgLabel (..), Type (..), TypeVarState (..), TypeVarM, TypeScheme (..), initialTypeVarState, newTypeVar, bindTypeVar, prune, unify, resolveType, freeTypeVars, generalize, instantiate, substituteTypes) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (State, evalState, gets, modify)
import Control.Monad.Trans
import qualified Data.Map
import Data.Maybe (isNothing)
import qualified Data.Set

-- id is a unique identifier for a typevar
-- thus same id -> same type variable
-- what a type var is bound to lives
-- in the substitution map in TypeVarState
newtype TypeVar = TypeVar {typeVarId :: Int}
  deriving (Show, Eq, Ord)

-- how an argument is passed to a function
data ArgLabel
  = NoLabel
  | Labeled String -- ~label
  | Optional String -- ?label
  deriving (Show, Eq)

-- TODO: polymorphic variants need row types
-- TODO: let-polymorphism; generalize let-bound types into type schemes
-- (forall a. a -> a) and instantiate them with fresh type vars at each use
data Type
  = -- basic units
    TUnit
  | TInt
  | TBool
  | TFloat
  | TChar
  | TString
  | -- things that might need to get resolved
    TArrow ArgLabel Type Type
  | TTuple [Type]
  | -- a named type constructor applied to its arguments,
    -- e.g. TCon "list" [TInt], TCon "option" [a], or a user-defined type
    TCon String [Type]
  | TVar TypeVar
  deriving (Show, Eq)

-- state needed while generating and binding type variables
data TypeVarState = TypeVarState
  { lastUsedTypeId :: Int,
    -- type var id: type it is bound to
    substitutions :: Data.Map.Map Int Type
  }

type TypeVarM = State TypeVarState

-- a type scheme (unconstrained, expr) is polymorphic over all instantiations
-- of the types in unconstrained
data TypeScheme = Forall (Data.Set.Set TypeVar) Type

initialTypeVarState :: TypeVarState
initialTypeVarState = TypeVarState {lastUsedTypeId = 0, substitutions = Data.Map.empty}

runTypeVarM :: TypeVarM a -> a
runTypeVarM m = evalState m initialTypeVarState

-- creates a fresh, unbound type variable with a unique id
newTypeVar :: TypeVarM TypeVar
newTypeVar = do
  modify (\s -> s {lastUsedTypeId = lastUsedTypeId s + 1})
  newId <- gets lastUsedTypeId
  return $ TypeVar {typeVarId = newId}

-- binds a type var to a type
-- assumes the type var is not already bound
bindTypeVar :: TypeVar -> Type -> TypeVarM ()
bindTypeVar var t =
  modify (\s -> s {substitutions = Data.Map.insert (typeVarId var) t (substitutions s)})

lookupTypeVar :: TypeVar -> TypeVarM (Maybe Type)
lookupTypeVar var = gets (Data.Map.lookup (typeVarId var) . substitutions)

-- equivalent to find in compilers book
-- follows bindings at the top level only, so the result is either
-- a non-type-var type or an unbound type var
prune :: Type -> TypeVarM Type
prune t = case t of
  TVar var -> do
    bound <- lookupTypeVar var
    case bound of
      Just t' -> prune t'
      Nothing -> return t
  _ -> return t

-- fully applies the substitutions, so the only type vars
-- left in the result are unbound ones
resolveType :: Type -> TypeVarM Type
resolveType t = do
  t' <- prune t
  case t' of
    TArrow argLabel from to -> TArrow argLabel <$> resolveType from <*> resolveType to
    TTuple ts -> TTuple <$> mapM resolveType ts
    TCon name ts -> TCon name <$> mapM resolveType ts
    _ -> return t'

-- unification somewhat based on the compilers book, but with adaptations
-- for the fact that instead of dealing with unary functions, we deal with
-- functions with multiple arguments
-- see page 397
unify :: Type -> Type -> ExceptT String TypeVarM ()
unify originalT1 originalT2 = do
  t1 <- lift $ prune originalT1
  t2 <- lift $ prune originalT2
  if t1 == t2
    then
      return ()
    else case (t1, t2) of
      (TArrow argLabel1 from1 to1, TArrow argLabel2 from2 to2) -> do
        -- unify children
        when (argLabel1 /= argLabel2) $ throwError $ "Cannot unify function that expects " ++ (show argLabel1) ++ " with function that expects " ++ (show argLabel2)
        () <- unify from1 from2
        () <- unify to1 to2
        return ()
      (TCon tName1 types1, TCon tName2 types2) -> do
        -- must have same name and zip together be unifiable
        when (tName1 /= tName2) $ throwError $ "Cannot unify type constructors " ++ tName1 ++ " and " ++ tName2
        when (length types1 /= length types2) $ throwError $ "Cannot unify type constructors " ++ tName1 ++ " and " ++ tName2 ++ " because have different lengths of parameters"
        mapM_ (\(a, b) -> unify a b) (zip types1 types2)
        return ()
      (TTuple types1, TTuple types2) -> do
        -- just like with con, but without the type name part
        when (length types1 /= length types2) $ throwError $ "Cannot unify tuples because have different lengths of parameters: " ++ (show $ length types1) ++ " vs " ++ (show $ length types2)
        mapM_ (\(a, b) -> unify a b) (zip types1 types2)
        return ()
      (TVar a, other) -> do
        -- since a is the result of doing a prune operation
        -- it shouldn't have a binding yet
        noBinding <- isNothing <$> (lift $ lookupTypeVar a)
        -- then bind a to it
        when (not noBinding) $ error "impossible to happen"
        checkNotRecursive a other
        lift $ bindTypeVar a other
        -- and then we are done
        return ()
      (other, TVar a) -> do
        -- same thing on this side
        noBinding <- isNothing <$> (lift $ lookupTypeVar a)
        when (not noBinding) $ error "impossible to happen"
        checkNotRecursive a other
        lift $ bindTypeVar a other
        return ()
      -- it's not a variable, and it's not a matching type, so therefore
      -- not unifiable
      _ -> throwError $ "Cannot unify types " ++ (show t1) ++ " and " ++ (show t2)

-- need to make sure that we don't have an infinite type
checkNotRecursive :: TypeVar -> Type -> ExceptT String TypeVarM ()
checkNotRecursive tVar t = do
  -- walk t and make sure tVar doesn't appear there
  t' <- lift $ prune t
  case t' of
    TUnit -> return ()
    TInt -> return ()
    TBool -> return ()
    TFloat -> return ()
    TChar -> return ()
    TString -> return ()
    TArrow _ t1 t2 -> do
      checkNotRecursive tVar t1
      checkNotRecursive tVar t2
      return ()
    TTuple types -> do
      mapM_ (checkNotRecursive tVar) types
      return ()
    TCon _ types -> do
      mapM_ (checkNotRecursive tVar) types
      return ()
    TVar otherVar ->
      if otherVar == tVar
        then
          throwError $ "Infinite type. The type variable " ++ (show tVar) ++ " appears inside the expression it would bind to"
        else return ()

-- see page 394 in compilers book for polymorphic functions
-- basically we just need to find any unbound variables and generalize them
-- find unconstrained type variables in the current context
freeTypeVars :: Type -> TypeVarM (Data.Set.Set TypeVar)
freeTypeVars tp = do
  pruned <- prune tp
  case pruned of
    TUnit -> return Data.Set.empty
    TInt -> return Data.Set.empty
    TBool -> return Data.Set.empty
    TFloat -> return Data.Set.empty
    TChar -> return Data.Set.empty
    TString -> return Data.Set.empty
    TArrow _ lhs rhs -> Data.Set.union <$> freeTypeVars lhs <*> freeTypeVars rhs
    TTuple types -> do
      freeVars <- mapM freeTypeVars types
      return $ foldl Data.Set.union Data.Set.empty freeVars
    TCon _ types -> do
      freeVars <- mapM freeTypeVars types
      return $ foldl Data.Set.union Data.Set.empty freeVars
    TVar var ->
      -- we know that we already pruned, so the fact that we have
      -- a variable here implies that it is unbound
      return $ Data.Set.singleton var

-- take a type and generalize all for sure unconstrained variables
-- unlike in the textbook where all functions are done first, we have an
-- environment with variables, so we need to subtract those
generalize :: Type -> Data.Set.Set TypeVar -> TypeVarM TypeScheme
generalize tp environmentVars = do
  freeVars <- freeTypeVars tp
  let trulyFree = Data.Set.difference freeVars environmentVars
  return $ Forall trulyFree tp

-- replaces the given (unbound) type variables with types
substituteTypes :: Data.Map.Map TypeVar Type -> Type -> TypeVarM Type
substituteTypes replacements tp = do
  pruned <- prune tp
  case pruned of
    TUnit -> return TUnit
    TInt -> return TInt
    TBool -> return TBool
    TFloat -> return TFloat
    TChar -> return TChar
    TString -> return TString
    TArrow arg lhs rhs -> TArrow arg <$> substituteTypes replacements lhs <*> substituteTypes replacements rhs
    TTuple types -> do
      TTuple <$> mapM (substituteTypes replacements) types
    TCon name types -> do
      TCon name <$> mapM (substituteTypes replacements) types
    TVar var ->
      case Data.Map.lookup var replacements of
        Just replacement -> return replacement
        Nothing -> return $ TVar var

-- use this ie when encountering an application of a polymorphic function
instantiate :: TypeScheme -> TypeVarM Type
instantiate (Forall unconstrained tp) = do
  let unconstrainedList = Data.Set.toList unconstrained
  freshVars <- mapM (const newTypeVar) unconstrainedList
  let replacements = zip unconstrainedList freshVars
  let replacementMap = Data.Map.fromList (map (fmap TVar) replacements)
  substituteTypes replacementMap tp
