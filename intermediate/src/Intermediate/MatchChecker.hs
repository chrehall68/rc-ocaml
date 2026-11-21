module Intermediate.MatchChecker where

import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty
import Data.Maybe (isNothing)
import Frontend.Tokens (IntLiteral (..))
import Intermediate.Type (Type (..))
import Intermediate.TypedAst

newtype ExhaustiveTProgram = ExhaustiveTProgram (TProgram Type)

collectErrors :: [Either String ()] -> Either String ()
collectErrors results =
  let (lefts, _) = partitionEithers results
   in case lefts of
        [] -> return ()
        _ -> Left $ unlines lefts

-- need to check that matches are exhaustive
-- we'll walk the same type tree
-- in the case of everything being exhaustive, nothing needs to be returned
-- otherwise, an error string of example cases not matched are returned
checkExhaustiveness :: TProgram Type -> Either String ExhaustiveTProgram
checkExhaustiveness prog@(TProgram definitions) =
  case partitionEithers $ map checkDefinition definitions of
    ([], _) -> return $ ExhaustiveTProgram prog
    (lefts, _) -> Left $ unlines lefts

checkDefinition :: TDefinition Type -> Either String ()
checkDefinition (TDefinitionType _) = return ()
checkDefinition (TDefinitionExpr expr) = checkExpr expr
checkDefinition (TDefinitionLet _ bindings) =
  collectErrors $ map (\(TLetBinding _ expr) -> checkExpr expr) bindings

checkExpr :: TExpr Type -> Either String ()
checkExpr expr = case expr of
  TEConstant _ _ -> return ()
  TEVar _ _ -> return ()
  TEVariant _ _ inner -> checkExpr inner
  TETuple _ e1 e2 rest -> collectErrors $ map checkExpr (e1 : e2 : rest)
  TEArray _ exprs -> collectErrors $ map checkExpr $ Data.List.NonEmpty.toList exprs
  TERecord _ (Just source) rest -> collectErrors $ map checkExpr $ source : (map snd rest)
  TERecord _ _ rest -> collectErrors $ map checkExpr $ map snd rest
  TEApply _ fn arg -> collectErrors $ map checkExpr [fn, arg]
  TEFieldAccess _ source _ -> checkExpr source
  TEFieldAssignment _ source _ val -> collectErrors $ map checkExpr [source, val]
  TEIf _ cond then_ else_ -> collectErrors $ map checkExpr [cond, then_, else_]
  TEWhile _ cond body -> collectErrors $ map checkExpr [cond, body]
  TEFor _ _ start end _ body -> collectErrors $ map checkExpr [start, end, body]
  TESequence _ left right -> collectErrors $ map checkExpr [left, right]
  -- TODO: patterns might be in the lhs of a let
  TELet _ _ bindings body -> collectErrors $ map checkExpr $ body : (map (\(TLetBinding _ expr) -> expr) bindings)
  -- deliberately applying match and function last
  TEFunction tp _ defaultExpr cases_ ->
    let caseExprs =
          concatMap
            ( \(TCase _ guard body) ->
                case guard of
                  Just g -> [g, body]
                  Nothing -> [body]
            )
            cases_
        otherItems = case defaultExpr of
          Just d -> d : caseExprs
          Nothing -> caseExprs
        otherResults = map checkExpr otherItems
     in collectErrors $ (checkCases tp cases_) : otherResults
  TEMatch _ matchedExpr cases_ ->
    checkCases (exprAnnotation matchedExpr) cases_
  TETry _ _ _ -> return () -- TODO

-- some guarded match may match values
-- but ocaml's compiler doesn't check that, so neither do we
checkCases :: Type -> [TCase Type] -> Either String ()
checkCases matchedTp cases_ =
  let unguarded = filter (\(TCase _ guard _) -> isNothing guard) cases_
   in checkCasesInner matchedTp unguarded

lookForPat :: [TCase Type] -> (TPattern Type -> Bool) -> Bool
lookForPat cases_ f = any (\(TCase pat _ _) -> f pat) cases_

-- type that we're matching, unguarded cases
-- returns Left (string of something missing), or ()
checkCasesInner :: Type -> [TCase Type] -> Either String ()
checkCasesInner matchedTyped cases_ =
  case matchedTyped of
    TUnit -> return () -- always ok
    TInt -> do
      let hasCatchall = lookForPat cases_ $ \pat -> case pat of
            TPWildcard _ -> True
            TPVariable _ _ -> True
            _ -> False
      case hasCatchall of
        True -> return ()
        False ->
          -- find a missing number
          -- guaranteed to eventually terminate since no one can write out all integers
          let findInt x = lookForPat cases_ $ \pat -> case pat of
                TPConstant _ (TConstInt (IntLiteral v _)) | v == x -> True
                _ -> False
              loop x =
                case findInt x of
                  True -> loop (x + 1)
                  False -> x
              missing = loop 0
           in Left $ show missing
    TBool -> do
      -- need true and false cases
      let hasTrue = lookForPat cases_ $ \pat -> case pat of
            TPWildcard _ -> True
            TPConstant _ (TConstBool True) -> True
            TPVariable _ _ -> True
            _ -> False
      let hasFalse = lookForPat cases_ $ \pat -> case pat of
            TPWildcard _ -> True
            TPConstant _ (TConstBool False) -> True
            TPVariable _ _ -> True
            _ -> False
      case (hasTrue, hasFalse) of
        (True, _) -> Left $ "true"
        (_, True) -> Left $ "false"
        _ -> return ()
    _ -> return ()
