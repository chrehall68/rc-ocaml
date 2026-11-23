module Intermediate.MatchChecker where

import Data.Either (partitionEithers)
import Data.List (find, intercalate, nub, (\\))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Debug.Trace (trace)
import Frontend.Tokens (IntLiteral (..))
import Intermediate.SymbolTable (ConstructorInfo (..), Sibling (Sibling))
import Intermediate.Type (Type (..), TypeVar)
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
  | WTuple [Witness]
  | WCon String (Maybe Witness)
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
  WTuple args -> "(" ++ (intercalate ", " $ map renderWitness args) ++ ")"
  WCon "::" (Just (WTuple [hd, tl])) -> renderOperand hd ++ " :: " ++ renderOperand tl
  WCon "::" (Just WAny) -> "_ :: _"
  WCon name Nothing -> name
  WCon name (Just arg) -> name ++ " " ++ renderOperand arg

-- parenthesize witnesses that would otherwise be ambiguous as an argument
renderOperand :: Witness -> String
renderOperand w = case w of
  WCon _ (Just _) -> "(" ++ renderWitness w ++ ")"
  _ -> renderWitness w

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
        TTuple types ->
          -- expand the tuple and update all things
          tupleAction types Nothing
        TCon _ typeArgs ->
          case headConstrs of
            [] -> do
              -- we know we had at least one pattern to us
              -- but the fact that we have no constructors means everything is a wildcard
              -- so it doesn't matter what we instantiate this as, we just skip ahead
              result <- checkUsefulness tailTypes remainingPats tailExamined
              return $ WAny : result
            (ConstructorInfo _ _ siblings _ _ _) : _ ->
              -- we have a constructor ref
              -- time to see if we span the space
              let haveNames = map (\(ConstructorInfo name _ _ _ _ _) -> name) headConstrs
                  maybeMissing = find (\(Sibling name _) -> not $ any (\s -> s == name) haveNames) siblings
               in case maybeMissing of
                    Nothing ->
                      -- we span the space, so try instantiating as any one of the constructors
                      let uniqueConstrs = nub headConstrs
                       in case find (isJust . snd) $ map (\constr -> (constr, setupTConCall typeArgs constr)) uniqueConstrs of
                            Just ((ConstructorInfo name _ _ _ args _), (Just witnesses)) ->
                              case args of
                                [] -> return $ WCon name Nothing : witnesses
                                -- several args were expanded as one tuple type, so they are one witness
                                _ -> return $ WCon name (Just $ head witnesses) : tail witnesses
                            Just _ -> error "impossible"
                            Nothing -> Nothing
                    Just (Sibling name hasArgument) -> do
                      -- we don't span the space
                      -- instantiate as that missing
                      result <- checkUsefulness tailTypes wildcardPats tailExamined
                      if hasArgument
                        then
                          return $ WCon name (Just WAny) : result
                        else
                          return $ WCon name Nothing : result
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
        TTuple types ->
          -- expand the tuple and update all things
          let TPTuple _ headItems = headExamined
           in tupleAction types (Just headItems)
        TCon _ typeArgs ->
          -- need to expand
          let constrInfo = case headExamined of
                TPVariant _ info _ -> info
                TPConstant _ (TConstVariant info) -> info
                _ -> error "impossible"
           in setupTConCall typeArgs constrInfo
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
                    -- replaces type params with the given types
    tupleAction types maybeHeadItems =
      let wildcards = map (\tp -> TPWildcard tp) types
          expander = \(pat : rest) -> case pat of
            TPWildcard _ ->
              -- gets expanded into a bunch  of _'s
              wildcards ++ rest
            TPVariable _ _ ->
              -- similar thing here
              wildcards ++ rest
            TPTuple _ pats ->
              pats ++ rest
            _ -> error "impossible"
          expanded = map expander remainingPats
          expandedTypes = types ++ tailTypes
          headItems = case maybeHeadItems of
            Just items -> items
            Nothing -> wildcards
       in case checkUsefulness expandedTypes expanded (headItems ++ tailExamined) of
            Nothing -> Nothing
            Just witnesses ->
              let (tupleArgs, rest) = splitAt (length types) witnesses
               in Just $ WTuple tupleArgs : rest

    setupTConCall typeArgs desiredConstr@(ConstructorInfo _ _ _ params declaredArgs _) =
      let -- the declared arg types mention the type's params (e.g. 'a),
          -- so replace them with this match's actual type args
          args = map (substParams (zip params typeArgs)) declaredArgs
          desiredTp = case args of
            [] -> []
            item : [] -> [item]
            lst -> [TTuple lst]
          desiredWildcards = map TPWildcard desiredTp
          expander = \(pat : rest) -> case pat of
            TPWildcard _ -> [desiredWildcards ++ rest]
            TPVariable _ _ -> [desiredWildcards ++ rest]
            TPConstant _ (TConstVariant info) | info == desiredConstr -> [rest]
            TPVariant _ info innerPat | info == desiredConstr -> [innerPat : rest]
            _ -> []
          expandedPats = concatMap expander remainingPats
          expandedTypes = desiredTp ++ tailTypes
          expandedExamined = desiredWildcards ++ tailExamined
       in checkUsefulness expandedTypes expandedPats expandedExamined

    extractConstr = \pat -> case pat of
      TPConstant _ (TConstVariant constrRef) -> [constrRef]
      TPVariant _ constrRef _ -> [constrRef]
      _ -> []
    headConstrs = concatMap extractConstr heads

substParams :: [(TypeVar, Type)] -> Type -> Type
substParams repl tp = case tp of
  TVar v | Just r <- lookup v repl -> r
  TArrow l a b -> TArrow l (substParams repl a) (substParams repl b)
  TTuple ts -> TTuple (map (substParams repl) ts)
  TCon n ts -> TCon n (map (substParams repl) ts)
  _ -> tp
