module Intermediate.TypeAnnotator (lowerAndInfer) where

import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.State (State, evalState, gets, runState, state)
import Data.Either (partitionEithers)
import qualified Frontend.Ast as Ast
import Intermediate.SymbolTable
import Intermediate.Type
import Intermediate.TypedAst

type OrError x = Either String x

data InferState = InferState {symbolTable :: SymbolTable, typeState :: TypeVarState}

-- like the previous State OrError, but without as much lifting required
type LowerAndInferM = ExceptT String (State InferState)

-- run a TypeVarM action against the type var state stored in InferState
liftTypes :: TypeVarM a -> LowerAndInferM a
liftTypes m = state $ \s ->
  let (a, ts) = runState m (typeState s) in (a, s {typeState = ts})

-- lower the parsed ast to our lower representation, and add types where possible
lowerAndInfer :: Ast.Program -> OrError (TProgram Type)
lowerAndInfer (Ast.Program _interface (Ast.UnitImplementation (implementation))) =
  -- ignore interface for now, then just translate implementation
  let items = case implementation of
        Just (Ast.ModuleItems moduleItems) -> moduleItems
        Nothing -> []
      inferState = InferState {symbolTable = empty, typeState = initialTypeVarState}
      results = evalState (mapM (runExceptT . lowerDefinitionOrExpr) items) inferState
      (lefts, rights) = partitionEithers results
   in case lefts of
        [] -> Right $ TProgram rights
        errors -> Left $ unlines errors

lowerDefinitionOrExpr :: Ast.DefinitionOrExpr -> LowerAndInferM (TDefinition Type)
lowerDefinitionOrExpr defOrExpr =
  case defOrExpr of
    Ast.Definition d -> lowerDefinition d
    Ast.Expr e -> TDefinitionExpr <$> lowerExpr e

typeConstant :: Ast.Constant -> LowerAndInferM Type
typeConstant constant =
  case constant of
    Ast.ConstantInt _ -> return TInt
    Ast.ConstantDouble _ -> return TDouble
    Ast.ConstantChar _ -> return TChar
    Ast.ConstantString _ -> return TString
    Ast.ConstantBool _ -> return TBool
    Ast.ConstantUnit -> return TUnit
    -- don't know what will be inside, so we store it as an 'a list / 'a array
    Ast.ConstantList -> do
      tvar <- liftTypes newTypeVar
      return $ TCon "list" [tvar]
    Ast.ConstantArray -> do
      tvar <- liftTypes newTypeVar
      return $ TCon "array" [tvar]
    -- TODO: implement these
    Ast.ConstantPolymorphicVariant _ -> throwError "polymorphic variants not implemented yet"
    Ast.ConstantVariant path -> do
      -- need to know where this variant name comes from
      symtab <- gets symbolTable
      info <- liftEither $ resolveConstructor symtab (show path)
      -- double check that it is a 0-arg constructor
      let expectedLength = length (constructorArgs info)
      if expectedLength == 0
        then return $ constructorResult info
        else throwError $ "constructor " ++ show path ++ " expects " ++ show expectedLength ++ " arguments, but was called with 0"

lowerExpr :: Ast.Expr -> LowerAndInferM (TExpr Type)
lowerExpr expr =
  case expr of
    Ast.ExprConstant constant -> do
      t <- typeConstant constant
      return $ TEConstant t constant
    _ -> throwError "failed"

lowerDefinition :: Ast.Definition -> LowerAndInferM (TDefinition Type)
lowerDefinition _def = throwError "failed"
