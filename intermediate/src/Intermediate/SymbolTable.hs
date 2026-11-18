module Intermediate.SymbolTable (SymbolTable, empty, resolve, insert, popScope, pushScope) where

import qualified Data.Map
import Intermediate.Type

-- symbol table just needs to be a quick way to look up
-- name: resolved type
type SymbolTable = [Data.Map.Map String Type]

empty :: SymbolTable
empty = [Data.Map.empty]

-- symbol operations
-- either succeeds, or errors with an error message
resolve :: SymbolTable -> String -> Either Type String
resolve table symbol = case table of
  [] -> Right $ "No definition found in scope for " ++ symbol
  scope : rest ->
    case Data.Map.lookup symbol scope of
      Just typeExpr -> Left typeExpr
      Nothing -> resolve rest symbol

insert :: SymbolTable -> String -> Type -> SymbolTable
insert table symbol typeExpr = case table of
  [] -> fail "Bad use of symbol table; inserting into no scopes"
  first : rest -> (Data.Map.insert symbol typeExpr first) : rest

-- scope operations
pushScope :: SymbolTable -> SymbolTable
pushScope table = Data.Map.empty : table

popScope :: SymbolTable -> SymbolTable
popScope table = case table of
  [] -> fail "Bad use of symbol table; should never happen"
  _ : rest -> rest
