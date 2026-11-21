module Intermediate.MatchChecker where

import Data.Either (partitionEithers)
import Data.List (find, intercalate)
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
  | WDouble Double
  | WChar Char
  | WString String
  deriving (Show, Eq)

data MatchError = MatchError
  { meExample :: Witness,
    -- whether the match had guarded cases (which we assume can fail)
    meHasGuards :: Bool
  }
  deriving (Show, Eq)

-- actually we'll just have wrapping things be in charge of doing
-- parentheses
renderWitness :: Witness -> String
renderWitness witness = case witness of
  WAny -> "_"
  WInt v -> show v
  WBool b -> if b then "true" else "false"
  WDouble d -> renderOCamlFloat d
  WChar c -> "'" ++ concatMap (escapeOCamlChar '\'') [c] ++ "'"
  WString s -> "\"" ++ concatMap (escapeOCamlChar '"') s ++ "\""

-- ocaml prints floats with a trailing "." instead of ".0", e.g. "0." or "1.5"
renderOCamlFloat :: Double -> String
renderOCamlFloat d
  | d == fromInteger whole && abs d < 1e15 = show whole ++ "."
  | otherwise = show d
  where
    whole = truncate d :: Integer

-- escape a character the way ocaml does inside a literal delimited by `quote`
escapeOCamlChar :: Char -> Char -> String
escapeOCamlChar quote c
  | c == quote = ['\\', c]
  | c == '\\' = "\\\\"
  | c == '\n' = "\\n"
  | c == '\t' = "\\t"
  | c == '\r' = "\\r"
  | c == '\b' = "\\b"
  | c >= ' ' && c <= '~' = [c]
  | otherwise = '\\' : pad (show (fromEnum c))
  where
    pad n = replicate (3 - length n) '0' ++ n

renderMatchError :: MatchError -> String
renderMatchError (MatchError w hasGuards) =
  "Pattern matching not exhaustive.\n"
    ++ "  Example of a value not matched: "
    ++ renderWitness w
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
    collectErrors $ checkCases tp cases_ : map checkExpr (maybe id (:) defaultExpr (caseExprs cases_))
  TEMatch _ matchedExpr cases_ ->
    collectErrors $ checkCases (exprAnnotation matchedExpr) cases_ : map checkExpr (matchedExpr : caseExprs cases_)
  TETry _ _ _ -> return () -- TODO

caseExprs :: [TCase Type] -> [TExpr Type]
caseExprs = concatMap (\(TCase _ guard body) -> maybe id (:) guard [body])

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
lookForPat cases_ f = any (\(TCase pat _ _) -> any f (flattenPat pat)) cases_

-- reduce a pattern to the alternatives it is made of
-- TODO: expand or-patterns here once they exist in the typed ast
flattenPat :: TPattern Type -> [TPattern Type]
flattenPat pat = case pat of
  TPAlias _ inner _ -> flattenPat inner
  _ -> [pat]

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
    TFloat
      | hasCatchall -> Nothing
      | otherwise ->
          -- same argument as ints here
          let used x = lookForPat cases_ $ \pat -> case pat of
                TPConstant _ (TConstDouble v) -> v == x
                _ -> False
           in Just $ WDouble $ until (not . used) (+ 1.0) 0.0
    TChar
      | hasCatchall -> Nothing
      | otherwise ->
          -- ocaml chars are single bytes, so all 256 can be written out
          let used x = lookForPat cases_ $ \pat -> case pat of
                TPConstant _ (TConstChar v) -> v == x
                _ -> False
           in case find (not . used) ['\0' .. '\255'] of
                Just v -> Just $ WChar v
                Nothing -> Nothing
    TString
      | hasCatchall -> Nothing
      | otherwise ->
          -- can't write out every string
          let used x = lookForPat cases_ $ \pat -> case pat of
                TPConstant _ (TConstString v) -> v == x
                _ -> False
           in Just $ WString $ until (not . used) ('a' :) ""
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
