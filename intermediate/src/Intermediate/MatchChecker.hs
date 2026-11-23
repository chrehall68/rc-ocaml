module Intermediate.MatchChecker where

import Data.Either (partitionEithers)
import Data.List (find, intercalate, nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, isNothing)
import Frontend.Ast (Path (..))
import Frontend.Tokens (IntLiteral (..))
import Intermediate.SymbolTable (ConstructorInfo (..), FieldInfo (..), Sibling (Sibling))
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
  | -- every field of the record, in declaration order
    WRecord [(String, Witness)]
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
  WRecord fields -> "{" ++ intercalate "; " [name ++ " = " ++ renderWitness w | (name, w) <- fields] ++ "}"
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
checkDefinition (TDefinitionLet _ bindings) = collectErrors $ map checkBinding bindings

-- the pattern of a let binding must match every value, like a single-case match
checkBinding :: TLetBinding Type -> Check
checkBinding (TLetBinding pat expr) =
  collectErrors [checkCases (patternAnnotation pat) [TCase pat Nothing expr], checkExpr expr]

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
  TELet _ _ bindings body -> collectErrors $ checkExpr body : map checkBinding bindings
  -- deliberately applying match and function last
  TEFunction _ _ defaultExpr cases_ ->
    -- the matched type is the parameter's, not the whole arrow type
    -- (every case's pattern has it, but a function always has at least one case)
    let paramTp = patternAnnotation (head [pat | TCase pat _ _ <- cases_])
     in collectErrors $ checkCases paramTp cases_ : map checkExpr (maybe id (:) defaultExpr (caseExprs cases_))
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
            [] | (recordInfo : _) <- [info | TPRecord _ info _ <- heads] -> recordAction typeArgs recordInfo Nothing
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
                       in case find (isJust . snd) $ map (\constr -> (constr, setupTConCall typeArgs constr Nothing)) uniqueConstrs of
                            Just (constr, (Just witnesses)) -> return $ wrapConstructor constr witnesses
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
      case (headType, flattenPat headExamined) of
        (TUnit, _) ->
          -- should be everything
          (WTuple [] :) <$> checkUsefulness tailTypes (map tail remainingPats) tailExamined
        (TInt, TPConstant _ (TConstInt (IntLiteral x _))) ->
          let nextPats = takeMatchingRests $ \pat -> case pat of
                TPConstant _ (TConstInt (IntLiteral v _)) -> v == x
                _ -> False
           in (WInt x :) <$> checkUsefulness tailTypes nextPats tailExamined
        (TBool, TPConstant _ (TConstBool x)) ->
          let nextPats = takeMatchingRests $ \pat -> case pat of
                TPConstant _ (TConstBool v) -> v == x
                _ -> False
           in (WBool x :) <$> checkUsefulness tailTypes nextPats tailExamined
        (TFloat, TPConstant _ (TConstDouble x)) ->
          let nextPats = takeMatchingRests $ \pat -> case pat of
                TPConstant _ (TConstDouble v) -> v == x
                _ -> False
           in (WDouble x :) <$> checkUsefulness tailTypes nextPats tailExamined
        (TChar, TPConstant _ (TConstChar x)) ->
          let nextPats = takeMatchingRests $ \pat -> case pat of
                TPConstant _ (TConstChar v) -> v == x
                _ -> False
           in (WChar x :) <$> checkUsefulness tailTypes nextPats tailExamined
        (TString, TPConstant _ (TConstString x)) ->
          let nextPats = takeMatchingRests $ \pat -> case pat of
                TPConstant _ (TConstString v) -> v == x
                _ -> False
           in (WString x :) <$> checkUsefulness tailTypes nextPats tailExamined
        (TArrow _ _ _, _) -> error "arrows should always be wildcards"
        (TTuple types, TPTuple _ headItems) ->
          -- expand the tuple and update all things
          tupleAction types (Just headItems)
        (TCon _ typeArgs, TPRecord _ info given) ->
          -- fields the pattern doesn't mention match anything
          let items = [maybe (TPWildcard tp) id (lookup name (givenFields given)) | (name, tp) <- zip (recordFields info) (recordTypes typeArgs info)]
           in recordAction typeArgs info (Just items)
        (TCon _ typeArgs, TPVariant _ info inner) -> setupTConCall typeArgs info (Just [inner]) >>= Just . wrapConstructor info
        (TCon _ typeArgs, TPConstant _ (TConstVariant info)) -> setupTConCall typeArgs info (Just []) >>= Just . wrapConstructor info
        _ -> error "impossible"
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
          expander = \(pat : rest) -> case flattenPat pat of
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

    -- a record is like a tuple of its fields
    -- maybeHeadItems are the examined pattern's field patterns, in declaration order
    recordAction typeArgs info maybeHeadItems =
      let names = recordFields info
          types = recordTypes typeArgs info
          wildcards = map TPWildcard types
          expander = \(pat : rest) -> case flattenPat pat of
            TPWildcard _ -> wildcards ++ rest
            TPVariable _ _ -> wildcards ++ rest
            TPRecord _ _ given ->
              -- fields the pattern doesn't mention match anything
              [ maybe wild id (lookup name (givenFields given))
              | (name, wild) <- zip names wildcards
              ]
                ++ rest
            _ -> error "impossible"
          headItems = maybe wildcards id maybeHeadItems
       in case checkUsefulness (types ++ tailTypes) (map expander remainingPats) (headItems ++ tailExamined) of
            Nothing -> Nothing
            Just witnesses ->
              let (fieldWitnesses, rest) = splitAt (length types) witnesses
               in Just $ WRecord (zip names fieldWitnesses) : rest

    -- maybeHeadItems is what the examined pattern holds in place of the constructor's argument
    setupTConCall typeArgs desiredConstr@(ConstructorInfo _ _ _ params declaredArgs _) maybeHeadItems =
      let -- the declared arg types mention the type's params (e.g. 'a),
          -- so replace them with this match's actual type args
          args = map (substParams (zip params typeArgs)) declaredArgs
          desiredTp = case args of
            [] -> []
            item : [] -> [item]
            lst -> [TTuple lst]
          desiredWildcards = map TPWildcard desiredTp
          expander = \(pat : rest) -> case flattenPat pat of
            TPWildcard _ -> [desiredWildcards ++ rest]
            TPVariable _ _ -> [desiredWildcards ++ rest]
            TPConstant _ (TConstVariant info) | info == desiredConstr -> [rest]
            TPVariant _ info innerPat | info == desiredConstr -> [innerPat : rest]
            _ -> []
          expandedPats = concatMap expander remainingPats
          expandedTypes = desiredTp ++ tailTypes
          expandedExamined = maybe desiredWildcards id maybeHeadItems ++ tailExamined
       in checkUsefulness expandedTypes expandedPats expandedExamined

    extractConstr = \pat -> case pat of
      TPConstant _ (TConstVariant constrRef) -> [constrRef]
      TPVariant _ constrRef _ -> [constrRef]
      _ -> []
    headConstrs = concatMap extractConstr heads

-- the types of a record's fields, in declaration order, for the given type arguments
recordTypes :: [Type] -> FieldInfo -> [Type]
recordTypes typeArgs info = map (substParams (zip (fieldParams info) typeArgs)) (recordFieldTypes info)

-- the patterns a record pattern gives, by field name
givenFields :: [(Path, TPattern Type)] -> [(String, TPattern Type)]
givenFields given = [(name, p) | (Path _ name, p) <- given]

-- extracts a constructor's witness from the witnesses of its argument columns
wrapConstructor :: ConstructorInfo -> [Witness] -> [Witness]
wrapConstructor (ConstructorInfo name _ _ _ args _) witnesses = case (args, witnesses) of
  ([], _) -> WCon name Nothing : witnesses
  -- args is nonempty, so we either have an item, or a TTuple of our args
  (_, w : rest) -> WCon name (Just w) : rest
  _ -> error "impossible"

-- returns the indices of the cases that can never match, by position, because the unguarded cases
-- above them already match everything they would
uselessCases :: Type -> [TCase Type] -> [Int]
uselessCases matchedTp cases_ =
  [ i
  | (i, TCase pat _ _) <- zip [0 ..] cases_,
    let earlier = [[flattenPat p] | TCase p Nothing _ <- take i cases_],
    isNothing (checkUsefulness [matchedTp] earlier [flattenPat pat])
  ]

substParams :: [(TypeVar, Type)] -> Type -> Type
substParams repl tp = case tp of
  TVar v | Just r <- lookup v repl -> r
  TArrow l a b -> TArrow l (substParams repl a) (substParams repl b)
  TTuple ts -> TTuple (map (substParams repl) ts)
  TCon n ts -> TCon n (map (substParams repl) ts)
  _ -> tp
