module Intermediate.TypeAnnotator (lowerAndInfer) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.State (State, evalState, gets, modify, runState, state)
import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NonEmpty
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
    Ast.ConstantDouble _ -> return TDouble
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
      info <- liftEither $ resolveConstructor symtab path
      -- double check that it is a 0-arg constructor
      case constructorArgs info of
        Nothing -> return $ constructorResult info
        Just expected -> throwError $ "constructor " ++ show path ++ " expects " ++ show expected ++ " as an argument, but was called without any arguments"

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
      tp <- liftEither $ resolveValue symtab p
      return $ TEVar tp p
    Ast.ExprNonconstantVariant {Ast.constr = constr, Ast.expr = expr} -> do
      symtab <- gets symbolTable
      constrInfo <- liftEither $ resolveConstructor symtab constr
      case constructorArgs constrInfo of
        Nothing -> throwError $ "constructor " ++ show constr ++ " expects no arguments, but was invoked on arguments"
        Just expected -> do
          typedExpr <- lowerExpr expr
          unifyOrRollback expected (exprAnnotation typedExpr)
          throwError "bad"
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
      opType <- liftEither $ resolveValue symtab (Ast.Path Nothing op)
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
    Ast.ExprLet {Ast.isRec = isRec, Ast.letBindings = letBindings, Ast.expr = inExpr} -> do
      -- then need to insert these bindings into the symbol table
      modify (\s -> s {symbolTable = pushScope (symbolTable s)})
      loweredBindings <- lowerAndInsertBindings isRec letBindings
      loweredInExpr <- lowerExpr inExpr
      -- then pop that scope before returning
      modify (\s -> s {symbolTable = popScope (symbolTable s)})
      return $ TELet (exprAnnotation loweredInExpr) isRec loweredBindings loweredInExpr
    _ -> throwError "Not implemented"

lowerDefinition :: Ast.Definition -> LowerAndInferM (TDefinition Type)
lowerDefinition def =
  case def of
    Ast.DefinitionType _typeDef -> throwError "type definitions not implemented"
    Ast.DefinitionLet isRec letBindings -> do
      loweredBindings <- lowerAndInsertBindings isRec letBindings
      return $ TDefinitionLet isRec loweredBindings

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
        Ast.LetBindingTyped {Ast.letValueName = name, Ast.params = params, Ast.typeExpression = _tpExpr, Ast.coercedType = _c, Ast.expr1 = expr} -> do
          -- TODO - handle type expressions and functions
          when (length params /= 0) $ throwError "functions not implemented"
          -- then evaluate the expression
          typedExpr <- lowerExpr expr
          let exprType = exprAnnotation typedExpr
          -- then just create and bind the type variable for our name
          tVar <- liftTypes newTypeVar
          liftTypes $ bindTypeVar tVar exprType
          let tp = TVar tVar
          modify (\s -> s {symbolTable = insertValue (symbolTable s) (Ast.Path Nothing name) tp})
          -- then that's our output
          return $ [TLetBinding (TPVariable tp name) (typedExpr)]