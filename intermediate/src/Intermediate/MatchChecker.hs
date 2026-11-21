module Intermediate.MatchChecker where

import Data.Either (partitionEithers)
import Data.List (intercalate)
import qualified Data.List.NonEmpty
import Data.Maybe (isNothing)
import Frontend.Tokens (IntLiteral (..))
import Intermediate.Type (Type (..))
import Intermediate.TypedAst

newtype ExhaustiveTProgram = ExhaustiveTProgram (TProgram Type)

-- an example value that a match fails to cover
data Witness
  = -- matches anything; used for sub-patterns that don't matter
    WAny
  | WInt Integer
  | WBool Bool
  deriving (Show, Eq)

data MatchError = MatchError
  { meExample :: Witness,
    -- whether the match had guarded cases (which we assume can fail)
    meHasGuards :: Bool
  }
  deriving (Show, Eq)

-- binding strength of the context a witness is printed in
-- (so that e.g. "Some (-1)" gets parenthesized once nested)
data ParenScope = TopScope | ArgScope

renderWitness :: ParenScope -> Witness -> String
renderWitness _ WAny = "_"
renderWitness prec (WInt n)
  | n < 0 = parenIf prec ("-" ++ show (abs n))
  | otherwise = show n
renderWitness _ (WBool b) = if b then "true" else "false"

parenIf :: ParenScope -> String -> String
parenIf ArgScope s = "(" ++ s ++ ")"
parenIf TopScope s = s

renderMatchError :: MatchError -> String
renderMatchError (MatchError w hasGuards) =
  "Pattern matching not exhaustive.\n"
    ++ "  Example of a value not matched: "
    ++ renderWitness TopScope w
    ++ (if hasGuards then "\n  (However, some guarded clause may match this value.)" else "")

-- all checks produce zero or more errors; Right () means no errors
type Check = Either [MatchError] ()

collectErrors :: [Check] -> Check
collectErrors results =
  case concat (fst (partitionEithers results)) of
    [] -> return ()
    errs -> Left errs

-- need to check that matches are exhaustive
-- we'll walk the same type tree
-- in the case of everything being exhaustive, nothing needs to be returned
-- otherwise, an error string of example cases not matched are returned
checkExhaustiveness :: TProgram Type -> Either String ExhaustiveTProgram
checkExhaustiveness prog@(TProgram definitions) =
  case collectErrors $ map checkDefinition definitions of
    Right () -> return $ ExhaustiveTProgram prog
    Left errs -> Left $ intercalate "\n" (map renderMatchError errs)

checkDefinition :: TDefinition Type -> Check
checkDefinition (TDefinitionType _) = return ()
checkDefinition (TDefinitionExpr expr) = checkExpr expr
checkDefinition (TDefinitionLet _ bindings) =
  collectErrors $ map (\(TLetBinding _ expr) -> checkExpr expr) bindings

checkExpr :: TExpr Type -> Check
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

-- guarded cases never count towards exhaustiveness (like ocaml), but we
-- remember that they exist so the error can mention them
checkCases :: Type -> [TCase Type] -> Check
checkCases matchedTp cases_ =
  let unguarded = filter (\(TCase _ guard _) -> isNothing guard) cases_
      hasGuards = length unguarded /= length cases_
   in case checkCasesInner matchedTp unguarded of
        Nothing -> return ()
        Just w -> Left [MatchError w hasGuards]

lookForPat :: [TCase Type] -> (TPattern Type -> Bool) -> Bool
lookForPat cases_ f = any (\(TCase pat _ _) -> f pat) cases_

-- type that we're matching, unguarded cases
-- returns Just (example of an unmatched value), or Nothing if exhaustive
checkCasesInner :: Type -> [TCase Type] -> Maybe Witness
checkCasesInner matchedTyped cases_ =
  case matchedTyped of
    TUnit -> Nothing -- always ok
    TInt
      | hasCatchall -> Nothing
      | otherwise ->
          -- find a missing number
          -- guaranteed to eventually terminate since no one can write out all integers
          let used x = lookForPat cases_ $ \pat -> case pat of
                TPConstant _ (TConstInt (IntLiteral v _)) -> v == x
                _ -> False
           in Just $ WInt $ until (not . used) (+ 1) 0
    TBool
      | not (matches True) -> Just (WBool True)
      | not (matches False) -> Just (WBool False)
      | otherwise -> Nothing
    _ -> Nothing
  where
    hasCatchall = lookForPat cases_ $ \pat -> case pat of
      TPWildcard _ -> True
      TPVariable _ _ -> True
      _ -> False
    matches b =
      hasCatchall
        || lookForPat
          cases_
          ( \pat -> case pat of
              TPConstant _ (TConstBool b') -> b == b'
              _ -> False
          )
