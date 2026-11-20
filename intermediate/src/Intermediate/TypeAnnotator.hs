module Intermediate.TypeAnnotator (lowerAndInfer) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (ExceptT, catchError, liftEither, runExceptT, throwError)
import Control.Monad.State (State, evalState, gets, modify, runState, state)
import Data.Either (isRight, partitionEithers)
import Data.List ((\\))
import Data.Foldable (foldrM)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map
import Data.Maybe (fromMaybe)
import qualified Data.Set
import qualified Frontend.Ast as Ast
import Intermediate.SymbolTable (envFreeVars)
import qualified Intermediate.SymbolTable as SymTab
import Intermediate.Type
import Intermediate.TypedAst

type OrError x = Either String x

-- namedTypeVars are the 'a-style variables written in the current top-level definition
-- they're shared across the whole definition, so every 'a means the same type variable
data InferState = InferState {symbolTable :: SymTab.SymbolTable, typeState :: TypeVarState, namedTypeVars :: Data.Map.Map String TypeVar}

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
      inferState = InferState {symbolTable = SymTab.empty, typeState = initialTypeVarState, namedTypeVars = Data.Map.empty}
      (results, finalState) = runState (mapM (runExceptT . lowerDefinitionOrExpr) items) inferState
      (lefts, rights) = partitionEithers results
   in case lefts of
        [] ->
          -- finalize any annotations
          return $ evalState (traverse resolveType (TProgram rights)) (typeState finalState)
        errors -> Left $ unlines errors

lowerDefinitionOrExpr :: Ast.DefinitionOrExpr -> LowerAndInferM (TDefinition Type)
lowerDefinitionOrExpr defOrExpr = do
  -- 'a only means the same thing within one top-level definition
  modify (\s -> s {namedTypeVars = Data.Map.empty})
  case defOrExpr of
    Ast.Definition d -> lowerDefinition d
    Ast.Expr e -> TDefinitionExpr <$> lowerExpr e

-- [] is just the constant constructor of the list type
lowerConstant :: Ast.Constant -> TConstant
lowerConstant constant =
  case constant of
    Ast.ConstantInt i -> TConstInt i
    Ast.ConstantDouble d -> TConstDouble d
    Ast.ConstantChar c -> TConstChar c
    Ast.ConstantString str -> TConstString str
    Ast.ConstantBool b -> TConstBool b
    Ast.ConstantUnit -> TConstUnit
    Ast.ConstantList -> TConstVariant (Ast.Path Nothing "[]")
    Ast.ConstantArray -> TConstEmptyArray
    Ast.ConstantPolymorphicVariant tag -> TConstPolymorphicVariant tag
    Ast.ConstantVariant path -> TConstVariant path

typeConstant :: TConstant -> LowerAndInferM Type
typeConstant constant =
  case constant of
    TConstInt _ -> return TInt
    TConstDouble _ -> return TFloat
    TConstChar _ -> return TChar
    TConstString _ -> return TString
    TConstBool _ -> return TBool
    TConstUnit -> return TUnit
    -- don't know what will be inside, so we store it as an 'a list / 'a array
    TConstEmptyArray -> do
      tvar <- liftTypes newTypeVar
      return $ TCon "array" [TVar tvar]
    -- TODO: implement these
    TConstPolymorphicVariant _ -> throwError "polymorphic variants not implemented yet"
    TConstVariant path -> do
      -- need to know where this variant name comes from
      symtab <- gets symbolTable
      info <- liftEither $ SymTab.resolveConstructor symtab path
      (args, result) <- instantiateConstructor info
      -- double check that it is a 0-arg constructor
      case args of
        [] -> return result
        expected -> throwError $ "constructor " ++ show path ++ " expects " ++ show expected ++ " as an argument, but was called without any arguments"

-- constructors of polymorphic types get fresh type variables at each use,
-- returns the argument types and the result type
instantiateConstructor :: SymTab.ConstructorInfo -> LowerAndInferM ([Type], Type)
instantiateConstructor info = do
  let params = Data.Set.fromList (SymTab.constructorParams info)
      -- instantiate everything together so they share the same fresh variables
      packed = TTuple (SymTab.constructorResult info : SymTab.constructorArgs info)
  unpacked <- liftTypes $ instantiate (Forall params packed)
  case unpacked of
    TTuple (result : args) -> return (args, result)
    _ -> error "impossible"

unifyOrRollback :: Type -> Type -> LowerAndInferM ()
unifyOrRollback a b = do
  originalState <- gets typeState
  unifies <- liftTypes $ runExceptT $ unify a b
  case unifies of
    Right () -> return ()
    Left err -> do
      -- rollback our state
      modify (\s -> s {typeState = originalState})
      throwError err

-- create a new symbol table and discard it on failure
-- on success, discard it only if `restoreOnSuccess` is true
withScopeRestore :: Bool -> LowerAndInferM a -> LowerAndInferM a
withScopeRestore restoreOnSuccess action = do
  savedTable <- gets symbolTable
  let restore = modify (\s -> s {symbolTable = savedTable})
  modify (\s -> s {symbolTable = SymTab.pushScope (symbolTable s)})
  result <- action `catchError` (\err -> restore >> throwError err)
  if restoreOnSuccess
    then
      restore
    else
      pure ()
  return result

-- run action and discard scope afterward
withScope :: LowerAndInferM a -> LowerAndInferM a
withScope action = withScopeRestore True action

-- unlabeled args (in call order), labeled args by label, optional (?label:) args by label
type CallArgs = ([Ast.Expr], Data.Map.Map String Ast.Expr, Data.Map.Map String Ast.Expr)

-- walks the function's arrow chain in parameter order, filling each parameter:
--   - positional parameters consume the next unlabeled argument
--   - labeled parameters look their argument up by label
--   - omitted optional parameters become None if a positional argument follows
applyCurriedFunction :: TExpr Type -> CallArgs -> LowerAndInferM (TExpr Type)
applyCurriedFunction curriedFunc args@(unlabeled, labeled, optional) = do
  funcType <- liftTypes $ prune (exprAnnotation curriedFunc)
  let noArgsLeft = null unlabeled && Data.Map.null labeled && Data.Map.null optional
      -- apply the function to an already-typed argument and continue
      applyTyped rhs typedArg remaining =
        applyCurriedFunction (TEApply rhs curriedFunc typedArg) remaining
      -- no argument for this parameter, but later ones have some
      etaExpand label lhs rhs = do
        v <- liftTypes newTypeVar
        let name = "__arg" ++ show (typeVarId v)
        body <- applyTyped rhs (TEVar lhs (Ast.Path Nothing name)) args
        return $ TEFunction (TArrow label lhs (exprAnnotation body)) label Nothing [TCase (TPVariable lhs name) Nothing body]
  case funcType of
    TArrow label lhs rhs
      -- partial application (or a complete one)
      | noArgsLeft -> return curriedFunc
      | otherwise -> case label of
          NoLabel -> case unlabeled of
            expr : rest -> do
              typedExpr <- lowerExpr expr
              unifyOrRollback lhs (exprAnnotation typedExpr)
              applyTyped rhs typedExpr (rest, labeled, optional)
            [] -> etaExpand label lhs rhs
          Labeled name -> case Data.Map.lookup name labeled of
            Just expr -> do
              typedExpr <- lowerExpr expr
              unifyOrRollback lhs (exprAnnotation typedExpr)
              applyTyped rhs typedExpr (unlabeled, Data.Map.delete name labeled, optional)
            Nothing -> etaExpand label lhs rhs
          Optional name ->
            case (Data.Map.lookup name optional, Data.Map.lookup name labeled) of
              -- ?name:e passes the option directly
              (Just expr, _) -> do
                typedExpr <- lowerExpr expr
                unifyOrRollback lhs (exprAnnotation typedExpr)
                applyTyped rhs typedExpr (unlabeled, labeled, Data.Map.delete name optional)
              -- ~name:e wraps the value in Some
              (Nothing, Just expr) -> do
                typedExpr <- lowerExpr expr
                unifyOrRollback lhs (TCon "option" [exprAnnotation typedExpr])
                let wrapped = TEVariant lhs (Ast.Path Nothing "Some") typedExpr
                applyTyped rhs wrapped (unlabeled, Data.Map.delete name labeled, optional)
              (Nothing, Nothing)
                | not (null unlabeled) -> do
                    -- a positional argument follows, so the optional is defaulted
                    let none = TEConstant lhs (TConstVariant (Ast.Path Nothing "None"))
                    applyTyped rhs none args
                | otherwise -> etaExpand label lhs rhs
    -- we don't know anything about the function (e.g. a lambda parameter),
    -- so ocaml assumes the call is purely positional
    TVar _
      | not (null unlabeled) && Data.Map.null labeled && Data.Map.null optional -> do
          argVar <- liftTypes newTypeVar
          resVar <- liftTypes newTypeVar
          unifyOrRollback funcType (TArrow NoLabel (TVar argVar) (TVar resVar))
          applyCurriedFunction curriedFunc args
    _
      -- successfully used all parameters
      | noArgsLeft -> return curriedFunc
      -- have remaining parameters but don't have a function type
      | otherwise -> throwError $ "Could not apply all arguments to lhs. Remaining arguments: " ++ show unlabeled ++ " and labeled " ++ show labeled ++ " and optional " ++ show optional ++ " to lhs"

consPath :: Ast.Path
consPath = Ast.Path Nothing "::"

-- a constructor applied to its (already lowered) argument
-- a constructor with several arguments takes them as one tuple
applyConstructor :: Ast.Path -> TExpr Type -> LowerAndInferM (TExpr Type)
applyConstructor constr typedArg = do
  symtab <- gets symbolTable
  constrInfo <- liftEither $ SymTab.resolveConstructor symtab constr
  (constrArgs, constrResult) <- instantiateConstructor constrInfo
  case constrArgs of
    [] -> throwError $ "constructor " ++ show constr ++ " expects no arguments, but was invoked on arguments"
    arg : [] -> unifyOrRollback arg (exprAnnotation typedArg)
    args -> unifyOrRollback (TTuple args) (exprAnnotation typedArg)
  return $ TEVariant constrResult constr typedArg

-- same, but for a constructor in a pattern
applyConstructorPattern :: Ast.Path -> TPattern Type -> LowerAndInferM (TPattern Type)
applyConstructorPattern constr typedInner = do
  symtab <- gets symbolTable
  constrInfo <- liftEither $ SymTab.resolveConstructor symtab constr
  (constrArgs, constrResult) <- instantiateConstructor constrInfo
  case constrArgs of
    [] -> throwError $ "constructor " ++ show constr ++ " expects no arguments, but was invoked on arguments"
    arg : [] -> unifyOrRollback arg (patternAnnotation typedInner)
    args -> unifyOrRollback (TTuple args) (patternAnnotation typedInner)
  return $ TPVariant constrResult constr typedInner

lowerExpr :: Ast.Expr -> LowerAndInferM (TExpr Type)
lowerExpr expr =
  case expr of
    Ast.ExprConstant rawConstant -> do
      let constant = lowerConstant rawConstant
      t <- typeConstant constant
      return $ TEConstant t constant
    Ast.ExprPath p -> do
      -- look up where this value comes from
      symtab <- gets symbolTable
      scheme <- liftEither $ SymTab.resolveValue symtab p
      -- polymorphic values get fresh type variables at each use
      tp <- liftTypes $ instantiate scheme
      return $ TEVar tp p
    Ast.ExprNonconstantVariant {Ast.constr = constr, Ast.expr = expr} -> do
      typedExpr <- lowerExpr expr
      applyConstructor constr typedExpr
    Ast.ExprNonconstantPolymorphicVariant _ _ -> do throwError "Polymorphic variants not supported yet"
    Ast.ExprAnnotated expr typeExpr -> do
      typedExpr <- lowerExpr expr
      desiredType <- lowerTypeExpr typeExpr
      unifyOrRollback (exprAnnotation typedExpr) desiredType
      return typedExpr
    Ast.ExprTuple first second rest -> do
      first' <- lowerExpr first
      second' <- lowerExpr second
      rest' <- mapM lowerExpr rest
      let tupleType = map exprAnnotation (first' : second' : rest')
      return $ TETuple (TTuple tupleType) first' second' rest'
    Ast.ExprCons elements -> do
      -- the last element is the tail, and everything before it is consed onto it
      typedElements <- mapM lowerExpr elements
      let cons h t = applyConstructor consPath (TETuple (TTuple [exprAnnotation h, exprAnnotation t]) h t [])
      foldrM cons (NonEmpty.last typedElements) (NonEmpty.init typedElements)
    Ast.ExprArray elements -> do
      typedElements <- mapM lowerExpr elements
      -- then all need to unify together
      innerVar <- liftTypes newTypeVar
      mapM_ (unifyOrRollback (TVar innerVar) . exprAnnotation) typedElements
      -- everything worked
      return $ TEArray (TCon "array" [TVar innerVar]) typedElements
    -- TODO: implement the many other expr types between ExprPath and ExprInfix
    Ast.ExprFunctionApplication func args -> do
      typedFunc <- lowerExpr func
      -- then the application needs to work
      -- ~x / ?x without a value are punned for ~x:x / ?x:x
      let punned name = maybe (Ast.ExprPath (Ast.Path Nothing name)) id
          (unlabeled, labeledList, optionalList) =
            foldr
              ( \arg (unl, lab, opt) -> case arg of
                  Ast.UnlabeledArgument e -> (e : unl, lab, opt)
                  Ast.LabeledArgument a b -> (unl, (a, punned a b) : lab, opt)
                  Ast.OptionalArgument a b -> (unl, lab, (a, punned a b) : opt)
              )
              ([], [], [])
              args
          labeled = Data.Map.fromList labeledList
          optional = Data.Map.fromList optionalList
      -- ocaml allows this for some reason, but we don't since it makes our life easier
      when (Data.Map.size labeled /= length labeledList || Data.Map.size optional /= length optionalList) $
        throwError "the same label was given more than once in a function application"
      applyCurriedFunction typedFunc (unlabeled, labeled, optional)
    -- TODO: Prefix
    Ast.ExprInfix op left right -> do
      symtab <- gets symbolTable
      opScheme <- liftEither $ SymTab.resolveValue symtab (Ast.Path Nothing op)
      opType <- liftTypes $ instantiate opScheme
      typedLeft <- lowerExpr left
      typedRight <- lowerExpr right
      -- then need to unify op with leftType -> rightType -> a
      outputType <- liftTypes newTypeVar
      let rhs = TArrow NoLabel (exprAnnotation typedRight) (TVar outputType)
          desiredType = TArrow NoLabel (exprAnnotation typedLeft) rhs
      () <- unifyOrRollback desiredType opType
      return $ TEApply (TVar outputType) (TEApply rhs (TEVar opType (Ast.Path Nothing op)) typedLeft) typedRight
    -- TODO: implement the other things in between as well
    Ast.ExprIf {Ast.cond = cond, Ast.thenExpr = thenExpr, Ast.elseExpr = maybeElseExpr} -> do
      -- need cond to resolve to boolean
      typedCond <- lowerExpr cond
      unifyOrRollback (exprAnnotation typedCond) TBool
      -- and the then/else need to unify too
      typedThen <- lowerExpr thenExpr
      let elseExpr = case maybeElseExpr of
            Just elseExpr -> elseExpr
            -- can only omit the "else" if the "if" evaluates to ()
            Nothing -> Ast.ExprConstant Ast.ConstantUnit
      typedElse <- lowerExpr elseExpr
      unifyOrRollback (exprAnnotation typedThen) (exprAnnotation typedElse)
      return $ TEIf (exprAnnotation typedThen) typedCond typedThen typedElse
    Ast.ExprWhile {Ast.cond = cond, Ast.body = expr} -> do
      -- technically the ocaml compiler will give a warning if body doesn't
      -- unify to unit. we opt to make it an error
      -- TODO: maybe support compiler warnings
      typedCon <- lowerExpr cond
      unifyOrRollback (exprAnnotation typedCon) TBool
      typedExpr <- lowerExpr expr
      unifyOrRollback (exprAnnotation typedExpr) TUnit
      return $ TEWhile TUnit typedCon typedExpr
    Ast.ExprFor {Ast.valueName = loopVar, Ast.start = start, Ast.end = end, Ast.body = body, Ast.isTo = isTo} -> do
      -- bounds must be ints
      typedStart <- lowerExpr start
      unifyOrRollback (exprAnnotation typedStart) TInt
      typedEnd <- lowerExpr end
      unifyOrRollback (exprAnnotation typedEnd) TInt
      -- the loop variable is an int, and is only visible inside the body
      typedBody <- withScope $ do
        modify (\s -> s {symbolTable = SymTab.insertValue (symbolTable s) (Ast.Path Nothing loopVar) TInt})
        typedBody <- lowerExpr body
        -- like while, we make a non-unit body an error instead of a warning
        unifyOrRollback (exprAnnotation typedBody) TUnit
        return typedBody
      return $ TEFor TUnit loopVar typedStart typedEnd isTo typedBody
    -- TODO: things between
    Ast.ExprSequence left right -> do
      -- both should be valid, but only result of right is returned
      -- and left should be unit (technically ocaml gives warnings if not)
      -- TODO: implement warnings
      loweredLeft <- lowerExpr left
      unifyOrRollback (exprAnnotation loweredLeft) TUnit
      loweredRight <- lowerExpr right
      return $ TESequence (exprAnnotation loweredRight) loweredLeft loweredRight
    Ast.ExprMatch {Ast.expr = expr, Ast.cases = cases} -> do
      -- every pattern must unify with our expr
      typedExpr <- lowerExpr expr
      let exprType = exprAnnotation typedExpr
      returnedTpVar <- liftTypes newTypeVar
      let returnedTp = TVar returnedTpVar
      typedCases <-
        mapM
          ( \(pat, guard, body) -> withScope $ do
              -- then for every case, we need to unify our expression with
              -- the pattern in question
              typedPat <- lowerAndInsertPattern pat
              unifyOrRollback (patternAnnotation typedPat) exprType
              -- and need to make sure the guard works and is a boolean
              typedGuard <- case guard of
                Just g -> do
                  typedG <- lowerExpr g
                  unifyOrRollback (exprAnnotation typedG) TBool
                  return $ Just typedG
                Nothing -> return Nothing
              -- and then need the body to work too and unify with our return type
              typedBody <- lowerExpr body
              unifyOrRollback (exprAnnotation typedBody) returnedTp
              return $ TCase typedPat typedGuard typedBody
          )
          cases
      return $ TEMatch returnedTp typedExpr typedCases
    Ast.ExprLet {Ast.isRec = isRec, Ast.letBindings = letBindings, Ast.expr = inExpr} -> do
      -- then need to insert these bindings into the symbol table
      -- (in their own scope, which is dropped after the body)
      (loweredBindings, loweredInExpr) <- withScope $ do
        loweredBindings <- lowerAndInsertBindings isRec letBindings
        loweredInExpr <- lowerExpr inExpr
        return (loweredBindings, loweredInExpr)
      return $ TELet (exprAnnotation loweredInExpr) isRec loweredBindings loweredInExpr
    Ast.ExprLambda {Ast.parameters = params, Ast.typeConstraint = retConstraint, Ast.body = body} ->
      lowerParameters params $ do
        typedBody <- lowerExpr body
        -- `fun x : t -> e` constrains the body, not the function
        case retConstraint of
          Just typeExpr -> do
            desired <- lowerTypeExpr typeExpr
            unifyOrRollback (exprAnnotation typedBody) desired
          Nothing -> return ()
        return typedBody
    _ -> throwError "Not implemented"

-- whether evaluating the expression can't have effects (so it's safe to generalize its type)
-- conservative: when unsure, say no (e.g. arrays and records could be mutable)
isNonExpansive :: TExpr t -> Bool
isNonExpansive expr =
  case expr of
    TEConstant _ _ -> True
    TEVar _ _ -> True
    TEVariant _ _ e -> isNonExpansive e
    TEPolymorphicVariant _ _ e -> isNonExpansive e
    TETuple _ a b rest -> all isNonExpansive (a : b : rest)
    -- a function is a value no matter what its body does
    TEFunction {} -> True
    _ -> False

-- lowers `fun p1 p2 -> body` into nested one-argument functions
-- each parameter gets its own scope, so later parameters (and the body) see earlier ones
-- `lowerBody` is run in the innermost scope
lowerParameters :: [Ast.Parameter] -> LowerAndInferM (TExpr Type) -> LowerAndInferM (TExpr Type)
lowerParameters [] lowerBody = lowerBody
lowerParameters (param : rest) lowerBody = withScope $ do
  (argLabel, typedDefault, typedPat, argType) <- lowerParameter param
  typedBody <- lowerParameters rest lowerBody
  return $ TEFunction (TArrow argLabel argType (exprAnnotation typedBody)) argLabel typedDefault [TCase typedPat Nothing typedBody]

-- label, default, pattern the parameter binds, and the type of the argument being passed
-- (which differs from the pattern's type for optional parameters with a default,
-- since the caller passes a `t option` but the body sees a `t`)
lowerParameter :: Ast.Parameter -> LowerAndInferM (ArgLabel, Maybe (TExpr Type), TPattern Type, Type)
lowerParameter param =
  case param of
    Ast.ParameterPattern pat -> do
      typedPat <- lowerAndInsertPattern pat
      return (NoLabel, Nothing, typedPat, patternAnnotation typedPat)
    Ast.ParameterLabel {Ast.label = name, Ast.parameterTypeConstraint = constraint, Ast.parameterPattern = pat} -> do
      -- ~x binds x
      typedPat <- lowerConstrainedPattern constraint (fromMaybe (Ast.PatternVariable name) pat)
      return (Labeled name, Nothing, typedPat, patternAnnotation typedPat)
    Ast.ParameterOptional {Ast.label = name, Ast.parameterTypeConstraint = constraint, Ast.parameterDefault = dflt} ->
      lowerOptionalParameter name (Ast.PatternVariable name) constraint dflt
    Ast.ParameterOptionalPattern {Ast.label = name, Ast.pattern = pat, Ast.parameterTypeConstraint = constraint, Ast.parameterDefault = dflt} ->
      lowerOptionalParameter name pat constraint dflt

lowerOptionalParameter :: String -> Ast.Pattern -> Maybe Ast.TypeExpr -> Maybe Ast.Expr -> LowerAndInferM (ArgLabel, Maybe (TExpr Type), TPattern Type, Type)
lowerOptionalParameter name pat constraint dflt = do
  -- the default can't see the variables this parameter binds, so lower it first
  typedDefault <- traverse lowerExpr dflt
  typedPat <- lowerConstrainedPattern constraint pat
  let patType = patternAnnotation typedPat
  argType <- case typedDefault of
    Just d -> do
      -- body sees a t, caller passes a t option
      unifyOrRollback patType (exprAnnotation d)
      return $ TCon "option" [patType]
    Nothing -> do
      -- without a default, the body sees the option itself
      inner <- liftTypes newTypeVar
      unifyOrRollback patType (TCon "option" [TVar inner])
      return patType
  return (Optional name, typedDefault, typedPat, argType)

lowerConstrainedPattern :: Maybe Ast.TypeExpr -> Ast.Pattern -> LowerAndInferM (TPattern Type)
lowerConstrainedPattern constraint pat =
  lowerAndInsertPattern $ maybe pat (Ast.PatternAnnotated pat) constraint

-- doesn't create a new scope; you should probably create a scope  before using it
-- if there are variables encountered, they'll be inserted into the current scope
-- if a variable is already bound in the current scope, this results in an error
lowerAndInsertPattern :: Ast.Pattern -> LowerAndInferM (TPattern Type)
lowerAndInsertPattern pat =
  case pat of
    Ast.PatternVariable name -> do
      tVar <- liftTypes newTypeVar
      let tp = TVar tVar
      let pathName = (Ast.Path Nothing name)
      symtab <- gets symbolTable
      let exists = isRight $ SymTab.resolveValueInLast symtab pathName
      when exists $ throwError $ "variable " ++ name ++ " bound multiple times in this pattern"
      modify (\s -> s {symbolTable = (SymTab.insertValue (symbolTable s) pathName tp)})
      return $ TPVariable tp name
    Ast.PatternWildcard -> do
      tVar <- liftTypes newTypeVar
      let tp = TVar tVar
      return $ TPWildcard tp
    Ast.PatternConstant rawConstant -> do
      let constant = lowerConstant rawConstant
      tp <- typeConstant constant
      return $ TPConstant tp constant
    Ast.PatternAlias innerPat name -> do
      typedInner <- lowerAndInsertPattern innerPat
      tVar <- liftTypes newTypeVar
      let tp = TVar tVar
      let pathName = (Ast.Path Nothing name)
      symtab <- gets symbolTable
      let exists = isRight $ SymTab.resolveValueInLast symtab pathName
      when exists $ throwError $ "variable " ++ name ++ " bound multiple times in this pattern"
      modify (\s -> s {symbolTable = (SymTab.insertValue (symbolTable s) pathName tp)})
      -- then need name to unify the name with the thing it's aliasing
      unifyOrRollback tp (patternAnnotation typedInner)
      return $ TPAlias tp typedInner name
    Ast.PatternAnnotated p typeExpr -> do
      typedPat <- lowerAndInsertPattern p
      desiredTp <- lowerTypeExpr typeExpr
      unifyOrRollback (patternAnnotation typedPat) desiredTp
      return typedPat
    -- TODO: handle PatternOr somehow; need to double check rules on names in different branches
    Ast.PatternVariant constrPath innerPat -> do
      typedInner <- lowerAndInsertPattern innerPat
      applyConstructorPattern constrPath typedInner
    -- TODO: handle PatternType?
    Ast.PatternTuple elements -> do
      typedElems <- mapM lowerAndInsertPattern elements
      let elemTypes = map patternAnnotation typedElems
      return $ TPTuple (TTuple elemTypes) typedElems
    Ast.PatternCons lhs rhs -> do
      typedLhs <- lowerAndInsertPattern lhs
      typedRhs <- lowerAndInsertPattern rhs
      applyConstructorPattern consPath (TPTuple (TTuple [patternAnnotation typedLhs, patternAnnotation typedRhs]) [typedLhs, typedRhs])
    Ast.PatternList elems -> do
      -- transform to cons with an empty list
      let toReduce = foldr (\lhs rhs -> Ast.PatternCons lhs rhs) (Ast.PatternConstant Ast.ConstantList) elems
      lowerAndInsertPattern toReduce
    -- TODO: handle the rest of the patterns
    _ -> throwError "bad"

-- TODO: handle these in a better way
extendedPathToPath :: Ast.ExtendedPath -> LowerAndInferM Ast.Path
extendedPathToPath (Ast.ExtendedPath modPath lastName) =
  case modPath of
    Nothing -> return $ Ast.Path Nothing lastName
    _ -> throwError "functors not supported yet"

lowerTypeExpr :: Ast.TypeExpr -> LowerAndInferM Type
lowerTypeExpr typeExpr =
  case typeExpr of
    Ast.TypeExprWildcard ->
      TVar <$> liftTypes newTypeVar
    Ast.TypeName extendedPath -> lowerTypeApplication [] extendedPath
    Ast.TypeVariable name -> do
      named <- gets namedTypeVars
      case Data.Map.lookup name named of
        Just var -> return $ TVar var
        Nothing -> do
          var <- liftTypes newTypeVar
          modify (\s -> s {namedTypeVars = Data.Map.insert name var (namedTypeVars s)})
          return $ TVar var
    Ast.TypeArrow lhs rhs -> do
      tLhs <- lowerTypeExpr lhs
      tRhs <- lowerTypeExpr rhs
      return $ TArrow NoLabel tLhs tRhs
    Ast.TypeTuple tupleTypes -> do
      types <- mapM lowerTypeExpr tupleTypes
      return $ TTuple types
    Ast.TypeApplication args extendedPath -> lowerTypeApplication args extendedPath

-- a type name applied to its arguments (possibly none), e.g. `int option`
lowerTypeApplication :: [Ast.TypeExpr] -> Ast.ExtendedPath -> LowerAndInferM Type
lowerTypeApplication argExprs extendedPath = do
  normalizedPath <- extendedPathToPath extendedPath
  symtab <- gets symbolTable
  SymTab.TypeInfo params body <- liftEither $ SymTab.resolveType symtab normalizedPath
  when (length params /= length argExprs) $
    throwError $
      "type " ++ show normalizedPath ++ " expects " ++ show (length params) ++ " argument(s), but was given " ++ show (length argExprs)
  args <- mapM lowerTypeExpr argExprs
  liftTypes $ substituteTypes (Data.Map.fromList (zip params args)) body

lowerDefinition :: Ast.Definition -> LowerAndInferM (TDefinition Type)
lowerDefinition def =
  case def of
    Ast.DefinitionLet isRec letBindings -> do
      loweredBindings <- lowerAndInsertBindings isRec letBindings
      return $ TDefinitionLet isRec loweredBindings
    Ast.DefinitionType definition@(Ast.TypeDefinition isNonrec typedefs) -> do
      case typedefs of
        [] -> error "impossible"
        _t1 : _t2 : _ -> throwError "multiple typedefs not implemented yet"
        td : [] -> lowerTypeDefinition isNonrec td
      return $ TDefinitionType definition

-- unless nonrec, the type can refer to itself
lowerTypeDefinition :: Bool -> Ast.TypeDef -> LowerAndInferM ()
lowerTypeDefinition isNonrec td = do
  let name = Ast.typedefName td
      paramNames = Ast.typedefParams td
  when (not (null (Ast.typedefConstraints td))) $ throwError "constraints not supported yet"
  when (length paramNames /= length (Data.Set.fromList paramNames)) $
    throwError $ "type " ++ name ++ " has a repeated type parameter"
  -- each parameter is a type variable, which 'a in the body refers to
  vars <- mapM (const $ liftTypes newTypeVar) paramNames
  let tp = TCon name (map TVar vars)
      isAlias = case (Ast.typedefEquation td, Ast.typedefRepresentation td) of
        (Just _, Nothing) -> True
        _ -> False
  -- a recursive type is its own distinct type, so it can be referred to
  -- before its body is lowered (an alias can't, since that would be cyclic)
  when (not isNonrec && not isAlias) $ registerType name (SymTab.TypeInfo vars tp)
  body <- withTypeParams td vars $ case (Ast.typedefEquation td, Ast.typedefRepresentation td) of
    (Nothing, Nothing) ->
      -- empty type, nothing to do
      return tp
    (Just eq, Nothing) ->
      -- acts as a type alias
      lowerTypeExpr eq
    (Nothing, Just (Ast.TypeVariant constructors)) -> do
      forM_ constructors $ \(Ast.ConstrDecl constrName typeExprs) -> do
        desiredTypes <- mapM lowerTypeExpr typeExprs
        let info = SymTab.ConstructorInfo {SymTab.constructorParams = vars, SymTab.constructorArgs = desiredTypes, SymTab.constructorResult = tp}
        modify (\s -> s {symbolTable = SymTab.insertConstructor (symbolTable s) (Ast.Path Nothing constrName) info})
      return tp
    (Nothing, Just (Ast.TypeRecord _fields)) -> throwError "todo"
    _ -> throwError "re-exporting types not supported yet"
  registerType name (SymTab.TypeInfo vars body)

-- runs an action where 'a in type expressions refers to the type's parameters
-- the body can only mention the declared parameters
withTypeParams :: Ast.TypeDef -> [TypeVar] -> LowerAndInferM a -> LowerAndInferM a
withTypeParams td vars action = do
  let paramNames = Ast.typedefParams td
  modify (\s -> s {namedTypeVars = Data.Map.fromList (zip paramNames vars)})
  result <- action
  used <- gets namedTypeVars
  case Data.Map.keys used \\ paramNames of
    [] -> return ()
    unbound : _ -> throwError $ "unbound type parameter '" ++ unbound ++ " in definition of " ++ Ast.typedefName td
  modify (\s -> s {namedTypeVars = Data.Map.empty})
  return result

registerType :: String -> SymTab.TypeInfo -> LowerAndInferM ()
registerType name info = modify (\s -> s {symbolTable = SymTab.insertType (symbolTable s) (Ast.Path Nothing name) info})

-- binds a name in the current scope, which must not already have it
bindMonomorphic :: String -> Type -> LowerAndInferM ()
bindMonomorphic name tp = do
  symtab <- gets symbolTable
  let pathName = Ast.Path Nothing name
  when (isRight $ SymTab.resolveValueInLast symtab pathName) $
    throwError $ "variable " ++ name ++ " bound multiple times in this binding"
  modify (\s -> s {symbolTable = SymTab.insertValue (symbolTable s) pathName tp})

-- what a let binding binds
data BindingTarget = BindName String | BindPattern Ast.Pattern

-- in the process of lowering the bindings, also inserts the bindings into the symbol table
-- in a recursive binding the name is visible (monomorphically) to its own right-hand side
-- afterwards the names are generalized (if the right-hand side is a value)
lowerAndInsertBindings :: Bool -> [Ast.LetBinding] -> LowerAndInferM [TLetBinding Type]
lowerAndInsertBindings isRec letBindings =
  case letBindings of
    [] -> error "impossible"
    _b1 : _b2 : _ -> throwError "Multiple bindings not implemented yet"
    binding : [] -> withScopeRestore False $ do
      let (target, valueExpr, annotation) = prepareBinding binding
      -- a recursive name is visible while lowering the right-hand side
      recVar <- case target of
        BindName name | isRec -> do
          var <- liftTypes newTypeVar
          bindMonomorphic name (TVar var)
          return $ Just var
        BindPattern _ | isRec -> throwError "only variables are allowed as the left-hand side of let rec"
        _ -> return Nothing
      typedExpr <- lowerExpr valueExpr
      let exprType = exprAnnotation typedExpr
      namesBefore <- gets (SymTab.currentScopeNames . symbolTable)
      (typedPat, names) <- case target of
        BindName name -> do
          tp <- case recVar of
            Just var -> do
              unifyOrRollback (TVar var) exprType
              return $ TVar var
            Nothing -> do
              var <- liftTypes newTypeVar
              unifyOrRollback (TVar var) exprType
              bindMonomorphic name (TVar var)
              return $ TVar var
          -- it should also conform to our type annotation
          forM_ annotation $ \tpExpr -> do
            desired <- lowerTypeExpr tpExpr
            unifyOrRollback desired tp
          return (TPVariable tp name, [name])
        BindPattern pat -> do
          typedPat <- lowerAndInsertPattern pat
          unifyOrRollback (patternAnnotation typedPat) exprType
          -- the pattern bound everything new in the current scope
          namesAfter <- gets (SymTab.currentScopeNames . symbolTable)
          return (typedPat, namesAfter \\ namesBefore)
      -- value restriction: only generalize if the right-hand side is a syntactic value
      -- (otherwise its type variables stay shared, like ocaml's weak '_a)
      when (isNonExpansive typedExpr) $ do
        symtab <- gets symbolTable
        -- generalize against everything outside this binding
        outerVars <- liftTypes $ envFreeVars (SymTab.popScope symtab)
        generalized <- liftTypes $ SymTab.generalizeNames outerVars names symtab
        modify (\s -> s {symbolTable = generalized})
      return [TLetBinding typedPat typedExpr]
  where
    -- with parameters, `let f p1 p2 : t = e` is sugar for `let f = fun p1 p2 : t -> e`,
    -- so the annotation constrains the result of the function and not f itself
    prepareBinding binding = case binding of
      Ast.LetBindingPattern pat expr -> (BindPattern pat, expr, Nothing)
      Ast.LetBindingTyped {Ast.letValueName = name, Ast.params = params, Ast.typeExpression = typeExpr, Ast.coercedType = _c, Ast.expr1 = expr} ->
        case params of
          [] -> (BindName name, expr, typeExpr)
          _ -> (BindName name, Ast.ExprLambda params typeExpr expr, Nothing)
