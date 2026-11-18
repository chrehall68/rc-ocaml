module Intermediate.Type where

import Control.Exception (assert)
import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.Map
import Data.Maybe (isNothing)

-- id is a unique identifier for a typevar
-- thus same id -> same type variable
-- what a type var is bound to lives
-- in the substitution map in TypeVarState
newtype TypeVar = TypeVar {typeVarId :: Int}
  deriving (Show, Eq)

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
  | TDouble
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
unify :: Type -> Type -> TypeVarM Bool
unify t1 t2 = do
  t1 <- prune t1
  t2 <- prune t2
  if t1 == t2
    then return True
    else case (t1, t2) of
      (TArrow argLabel1 from1 to1, TArrow argLabel2 from2 to2) -> do
        -- unify children
        result1 <- unify from1 from2
        result2 <- unify to1 to2
        return $ argLabel1 == argLabel2 && result1 && result2
      (TCon tName1 types1, TCon tName2 types2) -> do
        -- must have same name and zip together be unifiable
        unificationResults <- mapM (\(a, b) -> unify a b) (zip types1 types2)
        let childrenUnify = foldl (&&) True unificationResults
        return $ tName1 == tName2 && length types1 == length types2 && childrenUnify
      (TTuple types1, TTuple types2) -> do
        -- just like with con, but without the type name part
        unificationResults <- mapM (\(a, b) -> unify a b) (zip types1 types2)
        let childrenUnify = foldl (&&) True unificationResults
        return $ length types1 == length types2 && childrenUnify
      (TVar a, other) -> do
        -- since a is the result of doing a prune operation
        -- it shouldn't have a binding yet
        noBinding <- isNothing <$> lookupTypeVar a
        -- then bind a to it
        assert noBinding $ bindTypeVar a other
        -- and then we are done
        return True
      (other, TVar a) -> do
        -- same thing on this side
        noBinding <- isNothing <$> lookupTypeVar a
        assert noBinding $ bindTypeVar a other
        return True
      -- it's not a variable, and it's not a matching type, so therefore
      -- not unifiable
      _ -> return False
