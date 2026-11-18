module Intermediate.TypeAnnotator (lowerAndInfer) where

import Control.Monad.State (State, evalState, gets, modify, runState)
import Data.Either (partitionEithers)
import qualified Frontend.Ast as Ast
import Intermediate.SymbolTable
import Intermediate.Type
import Intermediate.TypedAst

type LowerAndInferM = State InferState

type OrError x = Either String x

type LowerOrError x = LowerAndInferM (OrError x)

lowerSuccess :: a -> LowerOrError a
lowerSuccess x = return $ return x

lowerFail :: String -> LowerOrError a
lowerFail x = return $ Left x

data InferState = InferState {symbolTable :: SymbolTable, typeState :: TypeVarState}

-- lower the parsed ast to our lower representation, and add types where possible
lowerAndInfer :: Ast.Program -> OrError (TProgram Type)
lowerAndInfer (Ast.Program _interface (Ast.UnitImplementation (implementation))) =
  -- ignore interface for now, then just translate implementation
  let items = case implementation of
        Just (Ast.ModuleItems moduleItems) -> moduleItems
        Nothing -> []
      inferState = InferState {symbolTable = empty, typeState = initialTypeVarState}
      results = evalState (mapM lowerDefinitionOrExpr items) inferState
      (lefts, rights) = partitionEithers results
   in case lefts of
        [] -> Right $ TProgram rights
        errors -> Left $ unlines errors

lowerDefinitionOrExpr :: Ast.DefinitionOrExpr -> LowerOrError (TDefinition Type)
lowerDefinitionOrExpr defOrExpr = do
  case defOrExpr of
    Ast.Definition d ->
      lowerDefinition d
    Ast.Expr e -> do
      texpr <- lowerExpr e
      return $ texpr >>= return . TDefinitionExpr

typeConstant :: Ast.Constant -> LowerOrError Type
typeConstant constant =
  case constant of
    Ast.ConstantInt _ -> lowerSuccess TInt
    Ast.ConstantDouble _ -> lowerSuccess TDouble
    Ast.ConstantChar _ -> lowerSuccess TChar
    Ast.ConstantString _ -> lowerSuccess TString
    Ast.ConstantBool _ -> lowerSuccess TBool
    Ast.ConstantUnit -> lowerSuccess TUnit
    Ast.ConstantList -> do
      -- don't know what will be in the list, so we store it as a 'a list
      tpState <- gets typeState
      let (tvar, nextState) = runState newTypeVar tpState
      modify (\state -> state {typeState = nextState})
      lowerSuccess $ TCon "list" [tvar]
    Ast.ConstantArray -> do
      -- like with lists, we don't know what will be inside
      tpState <- gets typeState
      let (tvar, nextState) = runState newTypeVar tpState
      modify (\state -> state {typeState = nextState})
      lowerSuccess $ TCon "array" [tvar]
    -- TODO: implement these
    Ast.ConstantPolymorphicVariant _ -> lowerFail "polymorphic variants not implemented yet"
    Ast.ConstantVariant path -> do
      -- need to know where this variant name comes from
      symtab <- gets symbolTable
      let constructorInfo = resolveConstructor symtab (show path)
      let resolvedType =
            constructorInfo >>= \info ->
              -- double check that it is a 0-arg constructor
              let args = constructorArgs info
                  expectedLength = length args
               in if expectedLength == 0
                    then
                      return $ constructorResult info
                    else
                      Left $ "constructor " ++ show path ++ " expects " ++ show expectedLength ++ " arguments, but was called with 0"

      return resolvedType

lowerExpr :: Ast.Expr -> LowerOrError (TExpr Type)
lowerExpr expr = do
  case expr of
    Ast.ExprConstant constant -> do
      t <- typeConstant constant
      return $ t >>= (\tp -> return $ TEConstant tp constant)
    _ -> lowerFail "failed"

lowerDefinition :: Ast.Definition -> LowerOrError (TDefinition Type)
lowerDefinition _def = do
  lowerFail "failed"