module Intermediate.SymbolTable
  ( SymbolTable,
    ConstructorInfo (..),
    FieldInfo (..),
    empty,
    resolve,
    insert,
    resolveConstructor,
    insertConstructor,
    resolveField,
    insertField,
    popScope,
    pushScope,
  )
where

import qualified Data.Map
import Intermediate.Type

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
    constructors :: Data.Map.Map String ConstructorInfo,
    fields :: Data.Map.Map String FieldInfo
  }

-- symbol table just needs to be a quick way to look up
-- name: resolved type / field info / constructor info
-- head is current (deepest) scope
type SymbolTable = [Scope]

emptyScope :: Scope
emptyScope = Scope {values = Data.Map.empty, constructors = Data.Map.empty, fields = Data.Map.empty}

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
            ("string_of_float", TArrow NoLabel TDouble TString),
            -- create ints
            ("int_of_string", TArrow NoLabel TString TInt),
            ("int_of_float", TArrow NoLabel TDouble TInt),
            -- create floats
            ("float_of_string", TArrow NoLabel TString TDouble),
            ("float_of_int", TArrow NoLabel TInt TDouble),
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
            ("+.", TArrow NoLabel TDouble (TArrow NoLabel TDouble TDouble)),
            ("-.", TArrow NoLabel TDouble (TArrow NoLabel TDouble TDouble)),
            ("*.", TArrow NoLabel TDouble (TArrow NoLabel TDouble TDouble)),
            ("/.", TArrow NoLabel TDouble (TArrow NoLabel TDouble TDouble))
          ],
      constructors = Data.Map.fromList [],
      fields = Data.Map.fromList []
    }

empty :: SymbolTable
empty = [initialScope]

-- generic lookup/insert for one namespace
-- either succeeds, or errors with an error message
resolveIn :: (Scope -> Data.Map.Map String a) -> String -> SymbolTable -> String -> Either String a
resolveIn namespace kind table symbol = case table of
  [] -> Left $ "No " ++ kind ++ " definition found in scope for " ++ symbol
  scope : rest ->
    case Data.Map.lookup symbol (namespace scope) of
      Just found -> Right found
      Nothing -> resolveIn namespace kind rest symbol

insertIn :: (Scope -> Data.Map.Map String a) -> (Scope -> Data.Map.Map String a -> Scope) -> SymbolTable -> String -> a -> SymbolTable
insertIn namespace setNamespace table symbol value = case table of
  [] -> error "Bad use of symbol table; inserting into no scopes"
  first : rest -> setNamespace first (Data.Map.insert symbol value (namespace first)) : rest

-- value operations
resolve :: SymbolTable -> String -> Either String Type
resolve = resolveIn values "value"

insert :: SymbolTable -> String -> Type -> SymbolTable
insert = insertIn values (\scope m -> scope {values = m})

-- constructor operations
resolveConstructor :: SymbolTable -> String -> Either String ConstructorInfo
resolveConstructor = resolveIn constructors "constructor"

insertConstructor :: SymbolTable -> String -> ConstructorInfo -> SymbolTable
insertConstructor = insertIn constructors (\scope m -> scope {constructors = m})

-- field operations
resolveField :: SymbolTable -> String -> Either String FieldInfo
resolveField = resolveIn fields "field"

insertField :: SymbolTable -> String -> FieldInfo -> SymbolTable
insertField = insertIn fields (\scope m -> scope {fields = m})

-- scope operations
pushScope :: SymbolTable -> SymbolTable
pushScope table = emptyScope : table

popScope :: SymbolTable -> SymbolTable
popScope table = case table of
  [] -> error "Bad use of symbol table; should never happen"
  _ : rest -> rest
