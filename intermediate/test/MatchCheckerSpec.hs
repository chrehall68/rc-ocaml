module MatchCheckerSpec (spec) where

import Data.List (isInfixOf)
import Frontend.Lexer (alexScanTokens)
import Frontend.Parser (parse)
import Intermediate.MatchChecker (checkExhaustiveness, uselessCases)
import Intermediate.TypedAst
import Intermediate.TypeAnnotator (lowerAndInfer)
import Test.Hspec

-- lex, parse, type check, and exhaustiveness check a whole source string
check :: String -> Either String ()
check src = do
  typed <- alexScanTokens src >>= parse >>= lowerAndInfer
  () <$ checkExhaustiveness typed

-- positions of the cases that can never match, in the last top-level match of the source
uselessIn :: String -> [Int]
uselessIn src = case alexScanTokens src >>= parse >>= lowerAndInfer of
  Right (TProgram defs) -> case last defs of
    TDefinitionExpr (TEMatch _ scrutinee cases_) -> uselessCases (exprAnnotation scrutinee) cases_
    _ -> error "the source must end with a match"
  Left err -> error err

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

  describe "let bindings and parameters" $ do
    it "accepts a variable binding" $
      accepted $
        check "let x = Some 1"
    it "accepts an irrefutable tuple binding" $
      accepted $
        check "let (a, b) = (1, true)"
    it "reports a refutable top-level binding" $
      rejectedWith ["not matched: None"] $
        check "let (Some y) = Some 1"
    it "reports a refutable let ... in binding" $
      rejectedWith ["not matched: None"] $
        check "let f z = let (Some y) = z in y"
    it "reports a refutable binding nested in a tuple" $
      rejectedWith ["not matched: (false, _)"] $
        check "let (true, a) = (true, 1)"
    it "reports a refutable function parameter" $
      rejectedWith ["not matched: None"] $
        check "let f (Some y) = y"
    it "reports a refutable parameter after an ordinary one" $
      rejectedWith ["not matched: false"] $
        check "let f a true = a"
    it "accepts an irrefutable constructor parameter" $
      accepted $
        check "type w = W of int;; let f (W n) = n"
    it "accepts a tuple parameter" $
      accepted $
        check "let f (a, b) = a + b"
    it "reports a refutable optional parameter with a default" $
      rejectedWith ["not matched: None"] $
        check "let f ?(x = Some 1) () = x;; let g ?x:(Some y = Some 1) () = y"

  describe "records" $ do
    it "accepts a record with every combination" $
      accepted $
        check "type r = {a: bool; b: bool};; let x = {a = true; b = true};; match x with | {a = true; b = _} -> () | {a = false; b = true} -> () | {a = false; b = false} -> ()"
    it "accepts a wildcard" $
      accepted $
        check "type r = {a: bool};; let x = {a = true};; match x with | _ -> ()"
    it "accepts a record pattern that mentions no fields' values" $
      accepted $
        check "type r = {a: bool; b: int};; let x = {a = true; b = 1};; match x with | {a = _; b = _} -> ()"
    it "accepts a pattern that leaves out fields" $
      accepted $
        check "type r = {a: bool; b: int};; let x = {a = true; b = 1};; match x with | {a = true} -> () | {a = false} -> ()"
    it "accepts a trailing wildcard" $
      accepted $
        check "type r = {a: bool; b: int};; let x = {a = true; b = 1};; match x with | {a = true; _} -> () | {b; _} -> ()"
    it "reports a missing value of the only field" $
      rejectedWith ["not matched: {a = false}"] $
        check "type r = {a: bool};; let x = {a = true};; match x with | {a = true} -> ()"
    it "reports a missing combination, naming every field" $
      rejectedWith ["not matched: {a = false; b = false}"] $
        check "type r = {a: bool; b: bool};; let x = {a = true; b = true};; match x with | {a = true; b = _} -> () | {a = false; b = true} -> ()"
    it "reports a missing combination when a field was left out" $
      rejectedWith ["not matched: {a = false; b = _}"] $
        check "type r = {a: bool; b: bool};; let x = {a = true; b = true};; match x with | {a = true} -> ()"
    it "reports a missing constant in an int field" $
      rejectedWith ["not matched: {a = true; b = 1}"] $
        check "type r = {a: bool; b: int};; let x = {a = true; b = 1};; match x with | {a = true; b = 0} -> () | {a = false; b = _} -> ()"
    it "instantiates the type parameter of a polymorphic record" $
      rejectedWith ["not matched: {contents = Some false}"] $
        check "type 'a box = {contents: 'a option};; let x = {contents = Some true};; match x with | {contents = None} -> () | {contents = Some true} -> ()"
    it "checks records nested in constructors" $
      rejectedWith ["not matched: Some {a = false}"] $
        check "type r = {a: bool};; let x = Some {a = true};; match x with | None -> () | Some {a = true} -> ()"
    it "checks constructors nested in records" $
      rejectedWith ["not matched: {a = None; b = _}"] $
        check "type r = {a: int option; b: bool};; let x = {a = None; b = true};; match x with | {a = Some _; b = _} -> ()"
    it "checks records nested in records" $
      rejectedWith ["not matched: {inner = {a = false}}"] $
        check "type i = {a: bool};; type o = {inner: i};; let x = {inner = {a = true}};; match x with | {inner = {a = true}} -> ()"
    it "checks records inside tuples" $
      rejectedWith ["not matched: ({a = false}, _)"] $
        check "type r = {a: bool};; let t = ({a = true}, 1);; match t with | ({a = true}, _) -> ()"
    it "handles aliases inside records and tuples" $
      accepted $
        check "type r = {a: bool};; let t = ({a = true}, 1);; match t with | (({a = _} as r), _) -> ()"
    it "reports a refutable record binding" $
      rejectedWith ["not matched: {a = false}"] $
        check "type r = {a: bool};; let {a = true} = {a = true}"
    it "accepts an irrefutable record binding" $
      accepted $
        check "type r = {a: bool; b: int};; let {a; b} = {a = true; b = 1}"
    it "does not let a guarded record clause make a match exhaustive" $
      rejectedWith [guardNote] $
        check "type r = {a: bool};; let x = {a = true};; match x with | {a = _} when true -> ()"

  describe "useless cases" $ do
    it "finds nothing when every case matters" $
      uselessIn "let x = 1;; match x with | 0 -> () | 1 -> () | _ -> ()" `shouldBe` []
    it "finds a case after a catch-all" $
      uselessIn "let x = 1;; match x with | _ -> () | 0 -> ()" `shouldBe` [1]
    it "finds a repeated constant" $
      uselessIn "let x = 1;; match x with | 0 -> () | 0 -> () | _ -> ()" `shouldBe` [1]
    it "finds a repeated bool" $
      uselessIn "let x = true;; match x with | true -> () | false -> () | true -> ()" `shouldBe` [2]
    it "finds every useless case" $
      uselessIn "let x = 1;; match x with | _ -> () | 0 -> () | 1 -> ()" `shouldBe` [1, 2]
    it "finds a case made redundant by several earlier cases together" $
      uselessIn "let x = true;; match x with | true -> () | false -> () | _ -> ()" `shouldBe` [2]
    it "does not let a guarded case rule out later ones" $
      uselessIn "let x = 1;; match x with | _ when true -> () | 0 -> ()" `shouldBe` []
    it "still checks a guarded case against the earlier ones" $
      uselessIn "let x = 1;; match x with | _ -> () | 0 when true -> ()" `shouldBe` [1]
    it "finds a repeated tuple" $
      uselessIn "let t = (true, 1);; match t with | (true, _) -> () | (_, 0) -> () | (true, 0) -> () | _ -> ()" `shouldBe` [2]
    it "finds a tuple covered column by column" $
      uselessIn "let t = (true, false);; match t with | (true, _) -> () | (false, _) -> () | (_, true) -> ()" `shouldBe` [2]
    it "keeps a tuple that is only partly covered" $
      uselessIn "let t = (true, 1);; match t with | (true, 0) -> () | (true, _) -> () | (false, _) -> ()" `shouldBe` []
    it "finds a repeated constructor" $
      uselessIn "let x = Some 1;; match x with | Some _ -> () | None -> () | Some 0 -> ()" `shouldBe` [2]
    it "finds a constructor argument that is already covered" $
      uselessIn "let x = Some true;; match x with | Some true -> () | Some false -> () | Some _ -> () | None -> ()" `shouldBe` [2]
    it "keeps a constructor case with a narrower argument" $
      uselessIn "let x = Some 1;; match x with | Some 0 -> () | Some _ -> () | None -> ()" `shouldBe` []
    it "finds a repeated constant constructor" $
      uselessIn "let x = Some 1;; match x with | None -> () | None -> () | Some _ -> ()" `shouldBe` [1]
    it "finds useless list cases" $
      uselessIn "let l = [1];; match l with | [] -> () | _ :: _ -> () | [_] -> ()" `shouldBe` [2]
    it "keeps list cases that split by length" $
      uselessIn "let l = [1];; match l with | [] -> () | [_] -> () | _ :: _ :: _ -> ()" `shouldBe` []
    it "finds a multi-argument constructor case already covered" $
      uselessIn "type t = P of int * bool | Q;; let x = Q;; match x with | P (_, _) -> () | Q -> () | P (0, true) -> ()" `shouldBe` [2]
    it "finds nested constructors that are already covered" $
      uselessIn "let x = Some (Some 1);; match x with | Some _ -> () | None -> () | Some (Some _) -> ()" `shouldBe` [2]
    it "sees through aliases" $
      uselessIn "let x = Some 1;; match x with | Some _ as y -> () | None -> () | Some 0 -> ()" `shouldBe` [2]
    it "finds a repeated record" $
      uselessIn "type r = {a: bool; b: bool};; let x = {a = true; b = true};; match x with | {a = true} -> () | {a = _; b = _} -> () | {a = true; b = false} -> ()" `shouldBe` [2]
    it "keeps record cases that differ in a field" $
      uselessIn "type r = {a: bool; b: bool};; let x = {a = true; b = true};; match x with | {a = true; b = true} -> () | {a = true; b = false} -> () | {a = false} -> ()" `shouldBe` []
    it "finds a record case made redundant by omitted fields" $
      uselessIn "type r = {a: bool; b: int};; let x = {a = true; b = 1};; match x with | {a = true} -> () | {a = true; b = 0} -> () | {a = false} -> ()" `shouldBe` [1]
    it "checks records inside constructors" $
      uselessIn "type r = {a: bool};; let x = Some {a = true};; match x with | Some {a = _} -> () | None -> () | Some {a = true} -> ()" `shouldBe` [2]
    it "checks constructors inside records" $
      uselessIn "type r = {a: int option};; let x = {a = None};; match x with | {a = Some _} -> () | {a = None} -> () | {a = Some 0} -> ()" `shouldBe` [2]
    it "checks strings and chars" $
      uselessIn "let s = \"a\";; match s with | \"a\" -> () | \"a\" -> () | _ -> ()" `shouldBe` [1]

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
