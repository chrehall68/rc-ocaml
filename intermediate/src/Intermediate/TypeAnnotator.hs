module Intermediate.TypeAnnotator (lowerAndInfer) where

import Control.Monad.State
import Data.Either (partitionEithers)
import qualified Frontend.Ast as Ast
import Intermediate.SymbolTable
import Intermediate.Type
import Intermediate.TypedAst

type LowerAndInferM = State InferState

data InferState = InferState {symbolTable :: SymbolTable}

-- lower the parsed ast to our lower representation, and add types where possible
lowerAndInfer :: Ast.Program -> Either String (TProgram Type)
lowerAndInfer (Ast.Program _interface (Ast.UnitImplementation (implementation))) =
  -- ignore interface for now, then just translate implementation
  let items = case implementation of
        Just (Ast.ModuleItems moduleItems) -> moduleItems
        Nothing -> []
      inferState = InferState {symbolTable = empty}
      results = evalState (mapM lowerDefinitionOrExpr items) inferState
      (lefts, rights) = partitionEithers results
   in case lefts of
        [] -> Right $ TProgram rights
        errors -> Left $ unlines errors

lowerDefinitionOrExpr :: Ast.DefinitionOrExpr -> LowerAndInferM (Either String (TDefinition Type))
lowerDefinitionOrExpr defOrExpr = do
  case defOrExpr of
    Ast.Definition d -> return $ Left "failed"
    Ast.Expr e -> return $ Left "Failed"