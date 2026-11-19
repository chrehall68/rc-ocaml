module Intermediate.SymbolTable
  ( SymbolTable,
    ConstructorInfo (..),
    FieldInfo (..),
    empty,
    resolveValue,
    insertValue,
    resolveType,
    insertType,
    resolveConstructor,
    insertConstructor,
    resolveField,
    insertField,
    popScope,
    pushScope,
  )
where

import qualified Data.Map
import Frontend.Ast (Path (..))
import Intermediate.Type (ArgLabel (..), Type (..), TypeVar (..))

-- what we know about a variant constructor, e.g. for
-- type 'a option = None | Some of 'a
-- Some has params ['a], args ['a], and result 'a option
-- params need to be replaced with fresh type vars at each use,
-- otherwise Some 1 and Some true would have to share a type
data ConstructorInfo = ConstructorInfo
  { constructorParams :: [TypeVar],
    -- empty for constant constructors
    constructorArgs :: [Type],
    constructorResult :: Type
  }
  deriving (Show, Eq)

-- what we know about a record field, e.g. for
-- type 'a box = {mutable contents : 'a}
-- contents has params ['a], type 'a, record 'a box, and is mutable
-- params get replaced with fresh type vars at each use, same as constructors
data FieldInfo = FieldInfo
  { fieldParams :: [TypeVar],
    fieldType :: Type,
    fieldRecord :: Type,
    fieldIsMutable :: Bool
  }
  deriving (Show, Eq)

-- yes, ocaml actually allows types / values to have same names
-- won't have same names as constructors though bc those are forced to be
-- uppercase
data Scope = Scope
  { values :: Data.Map.Map String Type,
    types :: Data.Map.Map String Type,
    constructors :: Data.Map.Map String ConstructorInfo,
    fields :: Data.Map.Map String FieldInfo
  }

-- symbol table just needs to be a quick way to look up
-- name: resolved type / field info / constructor info
-- head is current (deepest) scope
type SymbolTable = [Scope]

emptyScope :: Scope
emptyScope = Scope {values = Data.Map.empty, constructors = Data.Map.empty, fields = Data.Map.empty, types = Data.Map.empty}

-- predefined identifiers / constructs
-- TODO: maybe this should be dependency injected since
-- the converter will also need to know what's predefined...
initialScope :: Scope
initialScope =
  Scope
    { values =
        Data.Map.fromList
          [ -- create strings
            ("string_of_int", TArrow NoLabel TInt TString),
            ("string_of_bool", TArrow NoLabel TBool TString),
            ("string_of_float", TArrow NoLabel TFloat TString),
            -- create ints
            ("int_of_string", TArrow NoLabel TString TInt),
            ("int_of_float", TArrow NoLabel TFloat TInt),
            -- create floats
            ("float_of_string", TArrow NoLabel TString TFloat),
            ("float_of_int", TArrow NoLabel TInt TFloat),
            -- create bool
            ("bool_of_string", TArrow NoLabel TString TBool),
            -- TODO: do we need anything for chars?
            -- IO
            ("print_endline", TArrow NoLabel TString TUnit),
            ("read_line", TArrow NoLabel TUnit TString),
            -- operators
            ("+", TArrow NoLabel TInt (TArrow NoLabel TInt TInt)),
            ("-", TArrow NoLabel TInt (TArrow NoLabel TInt TInt)),
            ("*", TArrow NoLabel TInt (TArrow NoLabel TInt TInt)),
            ("/", TArrow NoLabel TInt (TArrow NoLabel TInt TInt)),
            ("+.", TArrow NoLabel TFloat (TArrow NoLabel TFloat TFloat)),
            ("-.", TArrow NoLabel TFloat (TArrow NoLabel TFloat TFloat)),
            ("*.", TArrow NoLabel TFloat (TArrow NoLabel TFloat TFloat)),
            ("/.", TArrow NoLabel TFloat (TArrow NoLabel TFloat TFloat)),
            -- TODO: this should probably be polymorphic
            ( "<",
              TArrow
                NoLabel
                (TVar (TypeVar (-1)))
                ( TArrow
                    NoLabel
                    (TVar (TypeVar (-2)))
                    TBool
                )
            )
          ],
      constructors = Data.Map.empty,
      fields = Data.Map.empty,
      types =
        Data.Map.fromList
          [ ("int", TInt),
            ("float", TFloat),
            ("bool", TBool),
            ("string", TString)
          ]
    }

empty :: SymbolTable
empty = [initialScope]

-- generic lookup/insert for one namespace
-- either succeeds, or errors with an error message
resolveIn :: (Scope -> Data.Map.Map String a) -> String -> SymbolTable -> Path -> Either String a
resolveIn namespace kind table symbol = case table of
  [] -> Left $ "No " ++ kind ++ " definition found in scope for " ++ symbolName
  scope : rest ->
    case Data.Map.lookup symbolName (namespace scope) of
      Just found -> Right found
      Nothing -> resolveIn namespace kind rest symbol
  where
    symbolName = pathToString symbol

insertIn :: (Scope -> Data.Map.Map String a) -> (Scope -> Data.Map.Map String a -> Scope) -> SymbolTable -> Path -> a -> SymbolTable
insertIn namespace setNamespace table symbol value = case table of
  [] -> error "Bad use of symbol table; inserting into no scopes"
  first : rest -> setNamespace first (Data.Map.insert symbolName value (namespace first)) : rest
  where
    symbolName = pathToString symbol

-- value operations
resolveValue :: SymbolTable -> Path -> Either String Type
resolveValue = resolveIn values "value"

insertValue :: SymbolTable -> Path -> Type -> SymbolTable
insertValue = insertIn values (\scope m -> scope {values = m})

-- value operations
resolveType :: SymbolTable -> Path -> Either String Type
resolveType = resolveIn types "type"

insertType :: SymbolTable -> Path -> Type -> SymbolTable
insertType = insertIn types (\scope m -> scope {types = m})

-- constructor operations
resolveConstructor :: SymbolTable -> Path -> Either String ConstructorInfo
resolveConstructor = resolveIn constructors "constructor"

insertConstructor :: SymbolTable -> Path -> ConstructorInfo -> SymbolTable
insertConstructor = insertIn constructors (\scope m -> scope {constructors = m})

-- field operations
resolveField :: SymbolTable -> Path -> Either String FieldInfo
resolveField = resolveIn fields "field"

insertField :: SymbolTable -> Path -> FieldInfo -> SymbolTable
insertField = insertIn fields (\scope m -> scope {fields = m})

-- scope operations
pushScope :: SymbolTable -> SymbolTable
pushScope table = emptyScope : table

popScope :: SymbolTable -> SymbolTable
popScope table = case table of
  [] -> error "Bad use of symbol table; should never happen"
  _ : rest -> rest

pathToString :: Path -> String
pathToString (Path prefixes name) =
  case prefixes of
    Nothing -> name
    Just _ -> error "Modules not implemented yet"