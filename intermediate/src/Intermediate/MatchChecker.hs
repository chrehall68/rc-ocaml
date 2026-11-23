module Intermediate.MatchChecker where

import Data.Either (partitionEithers)
import Data.List (find, intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
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
  TEArray _ exprs -> collectErrors $ map checkExpr $ NE.toList exprs
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
  let unguarded =
        concatMap
          ( \(TCase pat guard _) ->
              case guard of
                Nothing -> [[flattenPat pat]]
                _ -> []
          )
          cases_
      hasGuards = length unguarded /= length cases_
   in case checkUsefulness [matchedTp] unguarded [TPWildcard matchedTp] of
        Nothing -> return ()
        Just (w : []) -> Left [MatchError w hasGuards]
        Just _ -> error "impossible"

findUncoveredConstant :: [TPattern Type] -> [t] -> (t -> TPattern Type -> Bool) -> Maybe t
findUncoveredConstant patterns constantSpan isMatchedBy =
  find (\constant -> not $ any (isMatchedBy constant) patterns) constantSpan

-- reduce a pattern to the alternatives it is made of
-- TODO: expand or-patterns here once they exist in the typed ast
flattenPat :: TPattern Type -> TPattern Type
flattenPat pat = case pat of
  TPAlias _ inner _ -> flattenPat inner
  _ -> pat

isWildcard :: TPattern Type -> Bool
isWildcard pat = case flattenPat pat of
  TPWildcard _ -> True
  TPVariable _ _ -> True
  _ -> False

-- based on the find usefulness algorithm and the exhaustiveness check adaptation of it
-- https://www.cambridge.org/core/journals/journal-of-functional-programming/article/warnings-for-pattern-matching/3165B75113781E2431E3856972940347
-- types that need to be matched, patterns still available to do that matching, our pattern
-- witness is Just if useful, Nothing if not usefull
checkUsefulness :: [Type] -> [[TPattern Type]] -> [TPattern Type] -> Maybe [Witness]
-- no more types to narrow
checkUsefulness remainingTypes [] _ =
  -- no pats made it this far, so we successfully found a witness already.
  -- so, we can just fill the rest with wildcards since they don't matter
  Just $ map (const WAny) remainingTypes
checkUsefulness [] _ _ = Nothing
checkUsefulness (_ : _) (_ : _) [] = error "impossible"
-- have more remaining patterns and more remaining types to narrow
checkUsefulness (headType : tailTypes) remainingPats (headExamined : tailExamined)
  | isWildcard headExamined =
      -- is a wildcard, so look at the concrete constructors currently captured
      -- and then branch off of whether the concrete constructors spans the space
      case headType of
        TUnit ->
          -- should be everything
          checkUsefulness tailTypes (map tail remainingPats) tailExamined
        TInt ->
          -- should be some concrete constructor that doesn't exist here
          -- find that constructor
          let space = [0 ..]
              valueExtracter = \pat -> case pat of
                TPConstant _ (TConstInt (IntLiteral v _)) -> Just v
                _ -> Nothing
           in wildcardAction space valueExtracter WInt
        TBool ->
          let space = [True, False]
              valueExtracter = \pat -> case pat of
                TPConstant _ (TConstBool v) -> Just v
                _ -> Nothing
           in wildcardAction space valueExtracter WBool
        TFloat ->
          let space = [0 ..]
              valueExtracter = \pat -> case pat of
                TPConstant _ (TConstDouble v) -> Just v
                _ -> Nothing
           in wildcardAction space valueExtracter WDouble
        TChar ->
          let space = ['\0' .. '\255']
              valueExtracter = \pat -> case pat of
                TPConstant _ (TConstChar v) -> Just v
                _ -> Nothing
           in wildcardAction space valueExtracter WChar
        TString ->
          let space = map (\x -> replicate x 'a') [0 ..]
              valueExtracter = \pat -> case pat of
                TPConstant _ (TConstString v) -> Just v
                _ -> Nothing
           in wildcardAction space valueExtracter WString
        TArrow _ _ _ ->
          -- everything should be a wildcard, so just recurse
          checkUsefulness tailTypes (map tail remainingPats) tailExamined
        _ -> Nothing
  | otherwise =
      -- is a constructed thing
      -- so expand cur type, if possible, and continue matching
      -- on anything that also matches what we match
      case headType of
        TUnit ->
          -- should be everything
          checkUsefulness tailTypes (map tail remainingPats) tailExamined
        TInt ->
          let (TPConstant _ (TConstInt (IntLiteral x _))) = headExamined
              nextPats = takeMatchingRests $ \pat -> case pat of
                TPConstant _ (TConstInt (IntLiteral v _)) -> v == x
                _ -> False
           in checkUsefulness tailTypes nextPats tailExamined
        TBool ->
          let (TPConstant _ (TConstBool x)) = headExamined
              nextPats = takeMatchingRests $ \pat -> case pat of
                TPConstant _ (TConstBool v) -> v == x
                _ -> False
           in checkUsefulness tailTypes nextPats tailExamined
        TFloat ->
          let (TPConstant _ (TConstDouble x)) = headExamined
              nextPats = takeMatchingRests $ \pat -> case pat of
                TPConstant _ (TConstDouble v) -> v == x
                _ -> False
           in checkUsefulness tailTypes nextPats tailExamined
        TChar ->
          let (TPConstant _ (TConstChar x)) = headExamined
              nextPats = takeMatchingRests $ \pat -> case pat of
                TPConstant _ (TConstChar v) -> v == x
                _ -> False
           in checkUsefulness tailTypes nextPats tailExamined
        TString ->
          let (TPConstant _ (TConstString x)) = headExamined
              nextPats = takeMatchingRests $ \pat -> case pat of
                TPConstant _ (TConstString v) -> v == x
                _ -> False
           in checkUsefulness tailTypes nextPats tailExamined
        TArrow _ _ _ -> error "arrows should always be wildcards"
        _ -> Nothing
  where
    -- automatically includes wildcards
    takeMatchingRests f =
      concatMap
        ( \(rawPat : rest) ->
            let pat = flattenPat rawPat
             in if f pat
                  then [rest]
                  else case pat of
                    TPWildcard _ -> [rest]
                    TPVariable _ _ -> [rest]
                    _ -> []
        )
        remainingPats
    wildcardPats = takeMatchingRests (const False)
    heads = map (flattenPat . head) remainingPats
    wildcardAction space valueExtracter witnessConstr =
      let isMatching desired = \pat -> case valueExtracter pat of
            Just v -> v == desired
            Nothing -> False
          maybeMissing = findUncoveredConstant heads space isMatching
       in case maybeMissing of
            -- the constant constructors don't cover the full span
            -- as such, instantiate current wildcard with that, and recurse on the remaining wildcards
            Just missing -> do
              result <- checkUsefulness tailTypes wildcardPats tailExamined
              return $ witnessConstr missing : result
            Nothing ->
              -- all constant constructors are covered, so just exhaustively check
              -- if instantiating self as one of those would result in a witness
              let extracter desired = takeMatchingRests (isMatching desired)
               in case find (isJust . snd) $ map (\x -> (x, checkUsefulness tailTypes (extracter x) tailExamined)) space of
                    Just (x, Just witness) ->
                      -- found a witness that makes this pattern useful
                      Just $ witnessConstr x : witness
                    Just _ -> error "impossible"
                    Nothing -> Nothing -- not useful