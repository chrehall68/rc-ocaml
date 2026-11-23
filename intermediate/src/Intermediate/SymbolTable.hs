module Intermediate.SymbolTable
  ( SymbolTable,
    ConstructorInfo (..),
    TypeInfo (..),
    Sibling (..),
    builtinConstructor,
    variantConstructors,
    FieldInfo (..),
    empty,
    resolveValue,
    resolveValueInLast,
    insertValue,
    insertScheme,
    envFreeVars,
    generalizeNames,
    currentScopeNames,
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

import Control.Monad (foldM)
import qualified Data.Map
import qualified Data.Set
import Frontend.Ast (Path (..))
import Intermediate.Type (ArgLabel (..), Type (..), TypeScheme (..), TypeVar (..), TypeVarM, builtinTypeName, freeTypeVars, generalize)

-- what we know about a variant constructor, e.g. for
-- type 'a option = None | Some of 'a
-- Some has params ['a], args ['a], and result 'a option
-- params need to be replaced with fresh type vars at each use,
-- otherwise Some 1 and Some true would have to share a type
-- constructorTag is its position in the declaration (None = 0, Some = 1),
-- so two values of the same variant type are built by the same constructor
-- exactly when their tags match
-- constructorSiblings lists every constructor of the type (including this one)
-- in declaration order, which lets us notice when a match forgets one
data ConstructorInfo = ConstructorInfo
  { constructorName :: String,
    constructorTag :: Int,
    constructorSiblings :: [Sibling],
    constructorParams :: [TypeVar],
    -- empty for constant constructors
    constructorArgs :: [Type],
    constructorResult :: Type
  }
  deriving (Show, Eq)

-- just enough about a constructor to know its shape
data Sibling = Sibling
  { siblingName :: String,
    siblingHasArgument :: Bool
  }
  deriving (Show, Eq)

-- one of the predefined constructors (None, Some, [], ::), for code the compiler
-- generates itself. these are looked up in the initial scope, so a user redefining
-- the name can't change what they mean
builtinConstructor :: String -> ConstructorInfo
builtinConstructor name = case Data.Map.lookup name (constructors initialScope) of
  Just info -> info
  Nothing -> error $ "not a builtin constructor: " ++ name

-- builds the constructors of a variant type from their names and argument types
-- the result is in declaration order, so the tags are the positions
variantConstructors :: [TypeVar] -> Type -> [(String, [Type])] -> [ConstructorInfo]
variantConstructors params result decls =
  [ ConstructorInfo
      { constructorName = name,
        constructorTag = tag,
        constructorSiblings = siblings,
        constructorParams = params,
        constructorArgs = args,
        constructorResult = result
      }
  | (tag, (name, args)) <- zip [0 ..] decls
  ]
  where
    siblings = [Sibling name (not (null args)) | (name, args) <- decls]

-- what we know about a record field, e.g. for
-- type 'a box = {mutable contents : 'a}
-- contents has params ['a], type 'a, record 'a box, and is mutable
-- params get replaced with fresh type vars at each use, same as constructors
-- recordFields lists every field of the record (including this one), which
-- lets us notice when a record literal forgets one
data FieldInfo = FieldInfo
  { fieldParams :: [TypeVar],
    fieldType :: Type,
    fieldRecord :: Type,
    fieldIsMutable :: Bool,
    recordFields :: [String],
    -- the type of each of recordFields, in the same order (in terms of fieldParams)
    recordFieldTypes :: [Type]
  }
  deriving (Show, Eq)

-- what we know about a type name, e.g. for type 'a option = ...
-- the params are ['a] and the body is 'a option; writing `int option`
-- substitutes the arguments for the params in the body
-- (a plain type like int has no params)
-- typeConstructors is every constructor in declaration order,
-- empty unless this is a variant type
data TypeInfo = TypeInfo
  { typeParams :: [TypeVar],
    typeBody :: Type,
    typeConstructors :: [ConstructorInfo]
  }
  deriving (Show, Eq)

-- yes, ocaml actually allows types / values to have same names
-- won't have same names as constructors though bc those are forced to be
-- uppercase
data Scope = Scope
  { values :: Data.Map.Map String TypeScheme,
    types :: Data.Map.Map String TypeInfo,
    constructors :: Data.Map.Map String ConstructorInfo,
    fields :: Data.Map.Map String FieldInfo
  }

-- symbol table just needs to be a quick way to look up
-- name: resolved type / field info / constructor info
-- head is current (deepest) scope
type SymbolTable = [Scope]

emptyScope :: Scope
emptyScope = Scope {values = Data.Map.empty, constructors = Data.Map.empty, fields = Data.Map.empty, types = Data.Map.empty}

monomorphic :: Type -> TypeScheme
monomorphic = Forall Data.Set.empty

-- builtins that work at any type; the type variables are instantiated at each use
polymorphicValues :: Data.Map.Map String TypeScheme
polymorphicValues =
  let a = TypeVar (-1)
   in Data.Map.fromList
        [ ("<", Forall (Data.Set.singleton a) (TArrow NoLabel (TVar a) (TArrow NoLabel (TVar a) TBool))),
          ("==", Forall (Data.Set.singleton a) (TArrow NoLabel (TVar a) (TArrow NoLabel (TVar a) TBool)))
        ]

-- predefined identifiers / constructs
-- TODO: maybe this should be dependency injected since
-- the converter will also need to know what's predefined...
initialScope :: Scope
initialScope =
  Scope
    { values =
        Data.Map.union polymorphicValues $
          Data.Map.map monomorphic $
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
                ("mod", TArrow NoLabel TInt (TArrow NoLabel TInt TInt)),
                ("lxor", TArrow NoLabel TInt (TArrow NoLabel TInt TInt)),
                ("+.", TArrow NoLabel TFloat (TArrow NoLabel TFloat TFloat)),
                ("-.", TArrow NoLabel TFloat (TArrow NoLabel TFloat TFloat)),
                ("*.", TArrow NoLabel TFloat (TArrow NoLabel TFloat TFloat)),
                ("/.", TArrow NoLabel TFloat (TArrow NoLabel TFloat TFloat)),
                -- unary operators
                -- ocaml does this desugaring too (ie when you do -x that's desugared to ~-x)
                ("~-", TArrow NoLabel TInt TInt),
                ("~+", TArrow NoLabel TInt TInt),
                ("~-.", TArrow NoLabel TFloat TFloat),
                ("~+.", TArrow NoLabel TFloat TFloat)
              ],
      constructors =
        Data.Map.fromList [(constructorName c, c) | c <- listConstructors ++ optionConstructors],
      fields = Data.Map.empty,
      types =
        let a = TypeVar (-1)
            -- a builtin type constructor of one parameter
            unary name = TypeInfo [a] (TCon (builtinTypeName name) [TVar a]) []
         in Data.Map.fromList
              [ ("int", TypeInfo [] TInt []),
                ("float", TypeInfo [] TFloat []),
                ("bool", TypeInfo [] TBool []),
                ("string", TypeInfo [] TString []),
                ("unit", TypeInfo [] TUnit []),
                ("char", TypeInfo [] TChar []),
                ("list", (unary "list") {typeConstructors = listConstructors}),
                ("array", unary "array"),
                ("option", (unary "option") {typeConstructors = optionConstructors})
              ]
    }

-- lists and options are ordinary variant types
listConstructors :: [ConstructorInfo]
listConstructors =
  let a = TypeVar (-1)
      list = TCon (builtinTypeName "list") [TVar a]
   in variantConstructors [a] list [("[]", []), ("::", [TVar a, list])]

optionConstructors :: [ConstructorInfo]
optionConstructors =
  let a = TypeVar (-1)
   in variantConstructors [a] (TCon (builtinTypeName "option") [TVar a]) [("None", []), ("Some", [TVar a])]

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
resolveValue :: SymbolTable -> Path -> Either String TypeScheme
resolveValue = resolveIn values "value"

resolveValueInLast :: SymbolTable -> Path -> Either String TypeScheme
resolveValueInLast tab = resolveIn values "value" [head tab]

-- binds a name monomorphically (lambda parameters, pattern variables, ...)
insertValue :: SymbolTable -> Path -> Type -> SymbolTable
insertValue table path tp = insertScheme table path (monomorphic tp)

insertScheme :: SymbolTable -> Path -> TypeScheme -> SymbolTable
insertScheme = insertIn values (\scope m -> scope {values = m})

-- the names bound in the innermost scope
currentScopeNames :: SymbolTable -> [String]
currentScopeNames [] = []
currentScopeNames (current : _) = Data.Map.keys (values current)

-- generalizes the given monomorphic values of the innermost scope,
-- leaving out the given type variables (the ones something else still depends on)
generalizeNames :: Data.Set.Set TypeVar -> [String] -> SymbolTable -> TypeVarM SymbolTable
generalizeNames _ _ [] = error "Bad use of symbol table; generalizing no scopes"
generalizeNames envVars names (current : outer) = do
  generalized <- foldM generalizeOne (values current) names
  return $ current {values = generalized} : outer
  where
    generalizeOne vals name =
      case Data.Map.lookup name vals of
        -- already-polymorphic values have nothing more to generalize
        Just (Forall quantified tp) | Data.Set.null quantified -> do
          scheme <- generalize tp envVars
          return $ Data.Map.insert name scheme vals
        _ -> return vals

-- the unbound type variables that values in scope still depend on
-- these must not be generalized, since something else could still bind them
-- (variables a scheme quantifies over don't count; they get fresh copies at each use)
envFreeVars :: SymbolTable -> TypeVarM (Data.Set.Set TypeVar)
envFreeVars table = do
  perScheme <- mapM schemeFreeVars (concatMap (Data.Map.elems . values) table)
  return $ Data.Set.unions perScheme
  where
    schemeFreeVars (Forall quantified tp) =
      (`Data.Set.difference` quantified) <$> freeTypeVars tp

-- value operations
resolveType :: SymbolTable -> Path -> Either String TypeInfo
resolveType = resolveIn types "type"

insertType :: SymbolTable -> Path -> TypeInfo -> SymbolTable
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