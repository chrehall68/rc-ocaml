module MatchCheckerSpec (spec) where

import Data.List (isInfixOf)
import Frontend.Lexer (alexScanTokens)
import Frontend.Parser (parse)
import Intermediate.MatchChecker (checkExhaustiveness)
import Intermediate.TypeAnnotator (lowerAndInfer)
import Test.Hspec

-- lex, parse, type check, and exhaustiveness check a whole source string
check :: String -> Either String ()
check src = do
  typed <- alexScanTokens src >>= parse >>= lowerAndInfer
  () <$ checkExhaustiveness typed

-- the match is not exhaustive, and the error contains every given piece of text
rejectedWith :: [String] -> Either String () -> Expectation
rejectedWith texts (Left msg) = do
  msg `shouldSatisfy` ("Pattern matching not exhaustive" `isInfixOf`)
  mapM_ (\t -> msg `shouldSatisfy` (t `isInfixOf`)) texts
rejectedWith _ (Right ()) = expectationFailure "expected a non-exhaustive error, but the match was accepted"

accepted :: Either String () -> Expectation
accepted result = result `shouldBe` Right ()

guardNote :: String
guardNote = "some guarded clause may match"

spec :: Spec
spec = do
  describe "ints" $ do
    it "accepts a wildcard" $
      accepted $
        check "let x = 1;; match x with | 1 -> () | _ -> ()"
    it "accepts a variable catch-all" $
      accepted $
        check "let x = 1;; match x with | 1 -> () | y -> ()"
    it "accepts an alias of a catch-all" $
      accepted $
        check "let x = 1;; match x with | (_ as y) -> ()"
    it "reports the smallest missing non-negative int" $
      rejectedWith ["not matched: 1"] $
        check "let x = 1;; match x with | 0 -> ()"
    it "skips ints that are already covered" $
      rejectedWith ["not matched: 3"] $
        check "let x = 1;; match x with | 0 -> () | 1 -> () | 2 -> ()"

  describe "bools" $ do
    it "accepts true and false" $
      accepted $
        check "let b = true;; match b with | true -> () | false -> ()"
    it "accepts a wildcard" $
      accepted $
        check "let b = true;; match b with | true -> () | _ -> ()"
    it "reports a missing false" $
      rejectedWith ["not matched: false"] $
        check "let b = true;; match b with | true -> ()"
    it "reports a missing true" $
      rejectedWith ["not matched: true"] $
        check "let b = true;; match b with | false -> ()"

  describe "other constants" $ do
    it "accepts a catch-all on chars" $
      accepted $
        check "let c = 'a';; match c with | 'a' -> () | _ -> ()"
    it "reports a missing char, escaped like ocaml" $
      rejectedWith ["not matched: '\\000'"] $
        check "let c = 'a';; match c with | 'a' -> ()"
    it "reports a missing float with a trailing dot" $
      rejectedWith ["not matched: 1."] $
        check "let f = 1.0;; match f with | 0.0 -> ()"
    it "reports the shortest missing string" $
      rejectedWith ["not matched: \"aa\""] $
        check "let s = \"a\";; match s with | \"\" -> () | \"a\" -> () | \"hi\" -> ()"
    it "accepts unit" $
      accepted $
        check "let u = ();; match u with | () -> ()"

  describe "tuples" $ do
    it "accepts every combination of bools" $
      accepted $
        check "let t = (true, false);; match t with | (true, _) -> () | (false, true) -> () | (false, false) -> ()"
    it "accepts a wildcard tuple" $
      accepted $
        check "let t = (true, 1);; match t with | (_, _) -> ()"
    it "accepts a variable catch-all" $
      accepted $
        check "let t = (true, 1);; match t with | p -> ()"
    it "accepts overlapping rows that together cover everything" $
      accepted $
        check "let t = (true, 1);; match t with | (true, _) -> () | (_, 0) -> () | (false, n) -> ()"
    it "reports a missing combination" $
      rejectedWith ["not matched: (false, false)"] $
        check "let t = (true, false);; match t with | (true, _) -> () | (false, true) -> ()"
    it "reports a missing column value when the other column is an int" $
      rejectedWith ["not matched: (true, 1)"] $
        check "let t = (true, 1);; match t with | (false, _) -> () | (_, 0) -> ()"
    it "checks triples" $
      rejectedWith ["not matched: (false, false, 0)"] $
        check "let t = (true, false, 22);; match t with | (true, _, _) -> () | (_, true, 0) -> () | (false, _, 1) -> ()"
    it "accepts nested tuples" $
      accepted $
        check "let t = ((true, false), 1);; match t with | ((true, _), _) -> () | ((false, _), 0) -> () | (_, _) -> ()"
    it "reports a missing nested tuple" $
      rejectedWith ["not matched: ((false, _), _)"] $
        check "let t = ((true, false), 1);; match t with | ((true, _), _) -> ()"
    it "does not let a guarded tuple clause make a match exhaustive" $
      rejectedWith [guardNote] $
        check "let t = (true, false);; match t with | (_, _) when true -> ()"
    it "checks tuple matches inside function bodies" $
      rejectedWith ["not matched: (true, false)"] $
        check "let g a b = match (a, b) with | (true, true) -> 1 | (false, false) -> 2"

  describe "constructors" $ do
    it "accepts every constructor of an option" $
      accepted $
        check "let x = Some 3;; match x with | Some _ -> () | None -> ()"
    it "accepts a wildcard" $
      accepted $
        check "let x = Some 3;; match x with | _ -> ()"
    it "accepts a variable catch-all after a constructor" $
      accepted $
        check "let x = Some 3;; match x with | None -> () | y -> ()"
    it "reports a missing constant constructor" $
      rejectedWith ["not matched: None"] $
        check "let x = Some 3;; match x with | Some _ -> ()"
    it "reports a missing constructor with an argument" $
      rejectedWith ["not matched: Some _"] $
        check "let x = Some 3;; match x with | None -> ()"
    it "reports a missing value inside an argument" $
      rejectedWith ["not matched: Some 1"] $
        check "let x = Some 3;; match x with | Some 0 -> () | None -> ()"
    it "accepts an argument covered by several clauses" $
      accepted $
        check "let x = Some true;; match x with | Some true -> () | Some false -> () | None -> ()"
    it "reports a missing bool inside an argument" $
      rejectedWith ["not matched: Some false"] $
        check "let x = Some true;; match x with | Some true -> () | None -> ()"
    it "instantiates the type parameter for the argument" $
      rejectedWith ["not matched: Some (false, _)"] $
        check "let x = Some (true, 1);; match x with | Some (true, _) -> () | None -> ()"
    it "accepts lists" $
      accepted $
        check "let l = [1];; match l with | [] -> () | _ :: _ -> ()"
    it "reports a missing empty list" $
      rejectedWith ["not matched: []"] $
        check "let l = [1];; match l with | _ :: _ -> ()"
    it "reports a missing cons" $
      rejectedWith ["not matched: _ :: _"] $
        check "let l = [1];; match l with | [] -> ()"
    it "checks user-defined variants" $
      rejectedWith ["not matched: C"] $
        check "type t = A | B of int | C;; let x = B 1;; match x with | A -> () | B _ -> ()"
    it "accepts a user-defined variant that is fully covered" $
      accepted $
        check "type t = A | B of int | C;; let x = B 1;; match x with | A -> () | B _ -> () | C -> ()"
    it "checks constructors next to other columns" $
      rejectedWith ["not matched: (false, None)"] $
        check "let t = (true, Some 1);; match t with | (true, _) -> () | (false, Some _) -> ()"
    it "accepts a multi-argument constructor that is fully covered" $
      accepted $
        check "type t = P of int * bool | Q;; let x = Q;; match x with | P (_, true) -> () | P (_, false) -> () | Q -> ()"
    it "reports a missing combination in a multi-argument constructor" $
      rejectedWith ["not matched: P (0, false)"] $
        check "type t = P of int * bool | Q;; let x = Q;; match x with | P (_, true) -> () | Q -> ()"
    it "reports a missing int in a multi-argument constructor" $
      rejectedWith ["not matched: P (1, true)"] $
        check "type t = P of int * bool | Q;; let x = Q;; match x with | P (0, _) -> () | P (_, false) -> () | Q -> ()"
    it "accepts nested constructors that are fully covered" $
      accepted $
        check "let x = Some (Some 1);; match x with | Some (Some _) -> () | Some None -> () | None -> ()"
    it "reports a missing nested constructor" $
      rejectedWith ["not matched: Some None"] $
        check "let x = Some (Some 1);; match x with | Some (Some _) -> () | None -> ()"
    it "reports a missing value two constructors deep" $
      rejectedWith ["not matched: Some (Some 1)"] $
        check "let x = Some (Some 1);; match x with | Some (Some 0) -> () | Some None -> () | None -> ()"
    it "accepts overlapping constructor rows that together cover everything" $
      accepted $
        check "let x = Some 1;; match x with | Some 0 -> () | Some _ -> () | None -> ()"
    it "accepts a list matched by length" $
      accepted $
        check "let l = [1];; match l with | [] -> () | [_] -> () | _ :: _ :: _ -> ()"
    it "reports a missing list length" $
      rejectedWith ["not matched: 0 :: (_ :: _)"] $
        check "let l = [1];; match l with | [] -> () | [_] -> ()"
    it "reports a missing element value in a singleton list" $
      rejectedWith ["not matched: 1 :: []"] $
        check "let l = [1];; match l with | [] -> () | [0] -> () | _ :: _ :: _ -> ()"
    it "accepts a head-and-tail pattern with a catch-all tail" $
      accepted $
        check "let l = [true];; match l with | [] -> () | true :: _ -> () | false :: _ -> ()"
    it "reports a missing bool head with a recursive tail" $
      rejectedWith ["not matched: false :: _"] $
        check "let l = [true];; match l with | [] -> () | true :: _ -> ()"
    it "checks a list of options" $
      rejectedWith ["not matched: None :: _"] $
        check "let l = [Some 1];; match l with | [] -> () | Some _ :: _ -> ()"
    it "checks an option holding a list" $
      rejectedWith ["not matched: Some []"] $
        check "let x = Some [1];; match x with | None -> () | Some (_ :: _) -> ()"
    it "checks a user-defined recursive variant" $
      rejectedWith ["not matched: Node (Leaf, _)"] $
        check "type tree = Leaf | Node of tree * tree;; let t = Leaf;; match t with | Leaf -> () | Node (Node _, _) -> () | Node (_, _) when true -> ()"
    it "does not let a guarded constructor clause make a match exhaustive" $
      rejectedWith [guardNote] $
        check "let x = Some 3;; match x with | Some _ when true -> () | None -> ()"

  describe "guards" $ do
    it "does not let a guarded clause make a match exhaustive" $
      rejectedWith [] $
        check "let x = 1;; match x with | 1 -> () | y when y == 2 -> ()"
    it "mentions guarded clauses in the error" $
      rejectedWith [guardNote] $
        check "let x = 1;; match x with | 1 -> () | y when y == 2 -> ()"
    it "does not mention guards when there are none" $
      case check "let x = 1;; match x with | 1 -> ()" of
        Left msg -> msg `shouldNotSatisfy` (guardNote `isInfixOf`)
        Right () -> expectationFailure "expected an error"
    it "accepts when an unguarded clause covers everything" $
      accepted $
        check "let x = 1;; match x with | y when y == 2 -> () | _ -> ()"

  describe "nesting" $ do
    it "checks matches inside other expressions" $
      rejectedWith ["not matched: false"] $
        check "let f b = (match b with | true -> 1) + 1"
    it "checks matches inside match bodies" $
      rejectedWith ["not matched: 1"] $
        check "let x = 1;; match x with | _ -> (match x with | 0 -> ())"
    it "checks every definition and reports each failure" $
      case check "let a = (match true with | true -> ());; let b = (match false with | false -> ())" of
        Left msg -> do
          msg `shouldSatisfy` ("not matched: false" `isInfixOf`)
          msg `shouldSatisfy` ("not matched: true" `isInfixOf`)
        Right () -> expectationFailure "expected errors"
    it "checks the cases of a function" $
      rejectedWith ["not matched: false"] $
        check "let f = function | true -> 1"
