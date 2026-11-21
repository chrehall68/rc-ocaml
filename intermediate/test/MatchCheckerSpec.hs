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
      accepted $ check "let x = 1;; match x with | 1 -> () | _ -> ()"
    it "accepts a variable catch-all" $
      accepted $ check "let x = 1;; match x with | 1 -> () | y -> ()"
    it "accepts an alias of a catch-all" $
      accepted $ check "let x = 1;; match x with | (_ as y) -> ()"
    it "reports the smallest missing non-negative int" $
      rejectedWith ["not matched: 1"] $ check "let x = 1;; match x with | 0 -> ()"
    it "skips ints that are already covered" $
      rejectedWith ["not matched: 3"] $ check "let x = 1;; match x with | 0 -> () | 1 -> () | 2 -> ()"

  describe "bools" $ do
    it "accepts true and false" $
      accepted $ check "let b = true;; match b with | true -> () | false -> ()"
    it "accepts a wildcard" $
      accepted $ check "let b = true;; match b with | true -> () | _ -> ()"
    it "reports a missing false" $
      rejectedWith ["not matched: false"] $ check "let b = true;; match b with | true -> ()"
    it "reports a missing true" $
      rejectedWith ["not matched: true"] $ check "let b = true;; match b with | false -> ()"

  describe "other constants" $ do
    it "accepts a catch-all on chars" $
      accepted $ check "let c = 'a';; match c with | 'a' -> () | _ -> ()"
    it "reports a missing char, escaped like ocaml" $
      rejectedWith ["not matched: '\\000'"] $ check "let c = 'a';; match c with | 'a' -> ()"
    it "reports a missing float with a trailing dot" $
      rejectedWith ["not matched: 1."] $ check "let f = 1.0;; match f with | 0.0 -> ()"
    it "reports the shortest missing string" $
      rejectedWith ["not matched: \"aa\""] $
        check "let s = \"a\";; match s with | \"\" -> () | \"a\" -> () | \"hi\" -> ()"
    it "accepts unit" $
      accepted $ check "let u = ();; match u with | () -> ()"

  describe "guards" $ do
    it "does not let a guarded clause make a match exhaustive" $
      rejectedWith [] $ check "let x = 1;; match x with | 1 -> () | y when y == 2 -> ()"
    it "mentions guarded clauses in the error" $
      rejectedWith [guardNote] $ check "let x = 1;; match x with | 1 -> () | y when y == 2 -> ()"
    it "does not mention guards when there are none" $
      case check "let x = 1;; match x with | 1 -> ()" of
        Left msg -> msg `shouldNotSatisfy` (guardNote `isInfixOf`)
        Right () -> expectationFailure "expected an error"
    it "accepts when an unguarded clause covers everything" $
      accepted $ check "let x = 1;; match x with | y when y == 2 -> () | _ -> ()"

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
      rejectedWith ["not matched: false"] $ check "let f = function | true -> 1"
