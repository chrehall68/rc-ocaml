module Intermediate.Type where

import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.Map

-- id is a unique identifier for a typevar
-- thus same id -> same type variable
-- what a type var is bound to lives
-- in the substitution map in TypeVarState
newtype TypeVar = TypeVar {typeVarId :: Int}
  deriving (Show, Eq)

data Type
  = -- basic units
    TUnit
  | TInt
  | TBool
  | TFloat
  | -- things that might need to get resolved
    TArrow Type Type
  | TTuple [Type]
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
newTypeVar :: TypeVarM Type
newTypeVar = do
  modify (\s -> s {lastUsedTypeId = lastUsedTypeId s + 1})
  newId <- gets lastUsedTypeId
  return $ TVar TypeVar {typeVarId = newId}

-- binds a type var to a type
-- assumes the type var is not already bound
bindTypeVar :: TypeVar -> Type -> TypeVarM ()
bindTypeVar var t =
  modify (\s -> s {substitutions = Data.Map.insert (typeVarId var) t (substitutions s)})

lookupTypeVar :: TypeVar -> TypeVarM (Maybe Type)
lookupTypeVar var = gets (Data.Map.lookup (typeVarId var) . substitutions)
