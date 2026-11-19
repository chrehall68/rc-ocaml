module Intermediate.TypeAnnotator (lowerAndInfer) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.State (State, evalState, gets, modify, runState, state)
import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Frontend.Ast as Ast
import qualified Intermediate.SymbolTable as SymTab
import Intermediate.Type
import Intermediate.TypedAst

type OrError x = Either String x

data InferState = InferState {symbolTable :: SymTab.SymbolTable, typeState :: TypeVarState}

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
      inferState = InferState {symbolTable = SymTab.empty, typeState = initialTypeVarState}
      (results, finalState) = runState (mapM (runExceptT . lowerDefinitionOrExpr) items) inferState
      (lefts, rights) = partitionEithers results
   in case lefts of
        [] ->
          -- finalize any annotations
          return $ evalState (traverse resolveType (TProgram rights)) (typeState finalState)
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
    Ast.ConstantDouble _ -> return TFloat
    Ast.ConstantChar _ -> return TChar
    Ast.ConstantString _ -> return TString
    Ast.ConstantBool _ -> return TBool
    Ast.ConstantUnit -> return TUnit
    -- don't know what will be inside, so we store it as an 'a list / 'a array
    Ast.ConstantList -> do
      tvar <- liftTypes newTypeVar
      return $ TCon "list" [TVar tvar]
    Ast.ConstantArray -> do
      tvar <- liftTypes newTypeVar
      return $ TCon "array" [TVar tvar]
    -- TODO: implement these
    Ast.ConstantPolymorphicVariant _ -> throwError "polymorphic variants not implemented yet"
    Ast.ConstantVariant path -> do
      -- need to know where this variant name comes from
      symtab <- gets symbolTable
      info <- liftEither $ SymTab.resolveConstructor symtab path
      -- double check that it is a 0-arg constructor
      case SymTab.constructorArgs info of
        [] -> return $ SymTab.constructorResult info
        expected -> throwError $ "constructor " ++ show path ++ " expects " ++ show expected ++ " as an argument, but was called without any arguments"

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

lowerExpr :: Ast.Expr -> LowerAndInferM (TExpr Type)
lowerExpr expr =
  case expr of
    Ast.ExprConstant constant -> do
      t <- typeConstant constant
      return $ TEConstant t constant
    Ast.ExprPath p -> do
      -- look up where this value comes from
      symtab <- gets symbolTable
      tp <- liftEither $ SymTab.resolveValue symtab p
      return $ TEVar tp p
    Ast.ExprNonconstantVariant {Ast.constr = constr, Ast.expr = expr} -> do
      symtab <- gets symbolTable
      constrInfo <- liftEither $ SymTab.resolveConstructor symtab constr
      typedExpr <- lowerExpr expr
      case SymTab.constructorArgs constrInfo of
        [] -> throwError $ "constructor " ++ show constr ++ " expects no arguments, but was invoked on arguments"
        arg : [] -> do
          unifyOrRollback arg (exprAnnotation typedExpr)
        args -> do
          unifyOrRollback (TTuple args) (exprAnnotation typedExpr)
      return $ TEVariant (SymTab.constructorResult constrInfo) constr typedExpr
    Ast.ExprNonconstantPolymorphicVariant _ _ -> do throwError "Polymorphic variants not supported yet"
    -- TODO: annotated
    Ast.ExprTuple first second rest -> do
      first' <- lowerExpr first
      second' <- lowerExpr second
      rest' <- mapM lowerExpr rest
      let tupleType = map exprAnnotation (first' : second' : rest')
      return $ TETuple (TTuple tupleType) first' second' rest'
    Ast.ExprCons elements -> do
      typedElements <- mapM lowerExpr elements
      -- now we need the last element to be a list
      let lastElem = NonEmpty.last typedElements
      innerVar <- liftTypes newTypeVar
      let desiredType = TCon "list" [TVar innerVar]
      unifyOrRollback desiredType (exprAnnotation lastElem)
      -- and then we need all the elements to be unifiable to each other
      -- but don't include last one since that is type list 'a
      mapM_ (unifyOrRollback (TVar innerVar) . exprAnnotation) (NonEmpty.init typedElements)
      -- now construct cons from this
      return $ foldr (\newHead accum -> TECons desiredType newHead accum) lastElem (NonEmpty.init typedElements)
    Ast.ExprArray elements -> do
      typedElements <- mapM lowerExpr elements
      -- then all need to unify together
      innerVar <- liftTypes newTypeVar
      mapM_ (unifyOrRollback (TVar innerVar) . exprAnnotation) typedElements
      -- everything worked
      return $ TEArray (TCon "array" [TVar innerVar]) typedElements
    -- TODO: implement the many other expr types between ExprPath and ExprInfix
    Ast.ExprInfix op left right -> do
      symtab <- gets symbolTable
      opType <- liftEither $ SymTab.resolveValue symtab (Ast.Path Nothing op)
      typedLeft <- lowerExpr left
      typedRight <- lowerExpr right
      -- then need to unify op with leftType -> rightType -> a
      outputType <- liftTypes newTypeVar
      let rhs = TArrow NoLabel (exprAnnotation typedRight) (TVar outputType)
          desiredType = TArrow NoLabel (exprAnnotation typedLeft) rhs
      () <- unifyOrRollback desiredType opType
      return $ TEApply (TVar outputType) (TEApply rhs (TEVar opType (Ast.Path Nothing op)) NoLabel typedLeft) NoLabel typedRight
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
      let comparisonOp = if isTo then "<" else ">"
          endValName = loopVar ++ "$ending"
          transformed =
            ( Ast.ExprLet
                False
                [Ast.LetBindingTyped {Ast.letValueName = loopVar, Ast.params = [], Ast.typeExpression = Nothing, Ast.coercedType = Nothing, Ast.expr1 = start}]
                ( Ast.ExprLet
                    False
                    [Ast.LetBindingTyped {Ast.letValueName = endValName, Ast.params = [], Ast.typeExpression = Nothing, Ast.coercedType = Nothing, Ast.expr1 = end}]
                    (Ast.ExprWhile (Ast.ExprInfix {Ast.op = comparisonOp, Ast.left = Ast.ExprPath (Ast.Path Nothing loopVar), Ast.right = Ast.ExprPath (Ast.Path Nothing endValName)}) body)
                )
            )
      lowerExpr transformed
    -- TODO: things between
    Ast.ExprSequence left right -> do
      -- both should be valid, but only result of right is returned
      loweredLeft <- lowerExpr left
      loweredRight <- lowerExpr right
      return $ TESequence (exprAnnotation loweredRight) loweredLeft loweredRight
    Ast.ExprMatch {Ast.expr = expr, Ast.cases = cases} -> do
      -- every pattern must unify with our expr
      -- TODO
      throwError "bad"
    Ast.ExprLet {Ast.isRec = isRec, Ast.letBindings = letBindings, Ast.expr = inExpr} -> do
      -- then need to insert these bindings into the symbol table
      modify (\s -> s {symbolTable = SymTab.pushScope (symbolTable s)})
      loweredBindings <- lowerAndInsertBindings isRec letBindings
      loweredInExpr <- lowerExpr inExpr
      -- then pop that scope before returning
      modify (\s -> s {symbolTable = SymTab.popScope (symbolTable s)})
      return $ TELet (exprAnnotation loweredInExpr) isRec loweredBindings loweredInExpr
    _ -> throwError "Not implemented"

-- if there are variables encountered, they'll be inserted into the current scope
-- TODO: maybe need to unify pattern with the thing it's matching against?
lowerAndInsertPattern :: Ast.Pattern -> LowerAndInferM (TPattern Type)
lowerAndInsertPattern pat =
  case pat of
    Ast.PatternVariable name -> do
      tVar <- liftTypes newTypeVar
      let tp = TVar tVar
      modify (\s -> s {symbolTable = (SymTab.insertValue (symbolTable s) (Ast.Path Nothing name) tp)})
      return $ TPVariable tp name
    Ast.PatternWildcard -> do
      tVar <- liftTypes newTypeVar
      let tp = TVar tVar
      return $ TPWildcard tp
    Ast.PatternConstant constant -> do
      tp <- typeConstant constant
      return $ TPConstant tp constant
    Ast.PatternAlias innerPat name -> do
      typedInner <- lowerAndInsertPattern innerPat
      tVar <- liftTypes newTypeVar
      let tp = TVar tVar
      modify (\s -> s {symbolTable = (SymTab.insertValue (symbolTable s) (Ast.Path Nothing name) tp)})
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
      symtab <- gets symbolTable
      constrInfo <- liftEither $ SymTab.resolveConstructor symtab constrPath
      typedInner <- lowerAndInsertPattern innerPat
      case SymTab.constructorArgs constrInfo of
        [] -> throwError $ "constructor " ++ show constrPath ++ " expects no arguments, but was invoked on arguments"
        arg : [] -> do
          -- need inner pat to unify with expected type
          unifyOrRollback arg (patternAnnotation typedInner)
        args -> do
          unifyOrRollback (TTuple args) (patternAnnotation typedInner)
      return $ TPVariant (SymTab.constructorResult constrInfo) constrPath typedInner
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
    Ast.TypeName extendedPath -> do
      normalizedPath <- extendedPathToPath extendedPath
      -- look up in symbol table
      symtab <- gets symbolTable
      liftEither $ SymTab.resolveType symtab normalizedPath
    -- TODO: Ast.TypeVariable
    Ast.TypeArrow lhs rhs -> do
      tLhs <- lowerTypeExpr lhs
      tRhs <- lowerTypeExpr rhs
      return $ TArrow NoLabel tLhs tRhs
    Ast.TypeTuple tupleTypes -> do
      types <- mapM lowerTypeExpr tupleTypes
      return $ TTuple types
    -- TODO: type application
    _ -> throwError "bad"

lowerDefinition :: Ast.Definition -> LowerAndInferM (TDefinition Type)
lowerDefinition def =
  case def of
    Ast.DefinitionLet isRec letBindings -> do
      loweredBindings <- lowerAndInsertBindings isRec letBindings
      return $ TDefinitionLet isRec loweredBindings
    Ast.DefinitionType definition@(Ast.TypeDefinition isNonrec typedefs) -> do
      when (not isNonrec) $ throwError "recursive types not implemented"
      case typedefs of
        [] -> error "impossible"
        _t1 : _t2 : _ -> throwError "multiple typedefs not implemented yet"
        (Ast.TypeDef params name equation repr constraints) : [] -> do
          when (length params > 0) $ throwError "polymorphism not supported yet"
          when (length constraints > 0) $ throwError "constraints not supported yet"
          tVar <- liftTypes newTypeVar
          let tp = TVar tVar
              pathName = Ast.Path Nothing name
          case (equation, repr) of
            (Nothing, Nothing) ->
              -- empty type, nothing to do
              pure ()
            (Just eq, Nothing) -> do
              -- need to unify with desired
              desired <- lowerTypeExpr eq
              unifyOrRollback tp desired
            (Nothing, Just rep) ->
              case rep of
                Ast.TypeVariant constructors -> do
                  mapM_
                    ( \(Ast.ConstrDecl constrName typeExprs) -> do
                        desiredTypes <- mapM lowerTypeExpr typeExprs
                        let constrPathName = Ast.Path Nothing constrName
                            info = SymTab.ConstructorInfo {SymTab.constructorParams = [], SymTab.constructorArgs = desiredTypes, SymTab.constructorResult = tp}
                        modify (\s -> s {symbolTable = SymTab.insertConstructor (symbolTable s) constrPathName info})
                        return ()
                    )
                    constructors
                Ast.TypeRecord fields -> throwError "todo"
            (Just eq, Just rep) -> throwError "re-exporting types not supported yet"
          modify (\s -> s {symbolTable = SymTab.insertType (symbolTable s) pathName tp})
          return $ TDefinitionType definition

-- in the process of lowering the bindings, also inserts the bindings into the symbol table
lowerAndInsertBindings :: Bool -> [Ast.LetBinding] -> LowerAndInferM [TLetBinding Type]
lowerAndInsertBindings isRec letBindings = do
  -- TODO: handle recursive bindings and multiple assignments
  -- since multiple assignments allow forward refs
  when isRec $ throwError "recursive not implemented yet"
  case letBindings of
    [] -> error "impossible"
    _b1 : _b2 : _ -> throwError "Multiple bindings not implemented yet"
    binding : [] ->
      -- exactly 1 nonrecursive binding
      case binding of
        Ast.LetBindingPattern _pat _expr -> throwError "pattern lets not implemented yet"
        Ast.LetBindingTyped {Ast.letValueName = name, Ast.params = params, Ast.typeExpression = typeExpr, Ast.coercedType = _c, Ast.expr1 = expr} -> do
          -- TODO - handle type expressions and functions
          when (length params /= 0) $ throwError "functions not implemented"
          -- then evaluate the expression
          typedExpr <- lowerExpr expr
          let exprType = exprAnnotation typedExpr
          -- then just create and bind the type variable for our name
          tVar <- liftTypes newTypeVar
          liftTypes $ bindTypeVar tVar exprType
          let tp = TVar tVar
          modify (\s -> s {symbolTable = SymTab.insertValue (symbolTable s) (Ast.Path Nothing name) tp})
          -- but also it should conform to our typeExpr
          unifyWith <- case typeExpr of
            Just tpExpr -> lowerTypeExpr tpExpr
            Nothing -> return tp
          unifyOrRollback unifyWith tp
          -- then that's our output
          return $ [TLetBinding (TPVariable tp name) (typedExpr)]