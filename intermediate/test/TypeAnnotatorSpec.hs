module TypeAnnotatorSpec (spec) where

import Data.List (isInfixOf)
import Frontend.Lexer (alexScanTokens)
import Frontend.Parser (parse)
import Intermediate.Type
import Intermediate.TypeAnnotator (lowerAndInfer)
import Intermediate.TypedAst
import Test.Hspec

-- lex, parse, and type check a whole source string
infer :: String -> Either String (TProgram Type)
infer src = alexScanTokens src >>= parse >>= lowerAndInfer

-- the (resolved) type of every top level let-bound name, in order
bindingTypes :: String -> Either String [(String, Type)]
bindingTypes src = do
  TProgram defs <- infer src
  return [(name, t) | TDefinitionLet _ bs <- defs, TLetBinding (TPVariable t name) _ <- bs]

typeOf :: String -> String -> Either String Type
typeOf name src = do
  tps <- bindingTypes src
  maybe (Left $ "no binding named " ++ name) Right (lookup name tps)

-- fails with an error message containing the given text
failsWith :: String -> Either String a -> Bool
failsWith text (Left msg) = text `isInfixOf` msg
failsWith _ _ = False

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False

spec :: Spec
spec = do
  describe "basic expressions" $ do
    it "types arithmetic" $
      typeOf "x" "let x = 1 + 2" `shouldBe` Right TInt
    it "rejects mismatched operands" $
      infer "let x = 1 + \"a\"" `shouldSatisfy` isLeft'
    it "types if expressions" $
      typeOf "x" "let x = if true then 1 else 2" `shouldBe` Right TInt
    it "rejects non-bool conditions" $
      infer "let x = if 1 then 1 else 2" `shouldSatisfy` isLeft'
    it "rejects if branches of different types" $
      infer "let x = if true then 1 else \"a\"" `shouldSatisfy` isLeft'
    it "types a let annotation" $
      typeOf "x" "let x : int = 1" `shouldBe` Right TInt
    it "rejects a wrong let annotation" $
      infer "let x : string = 1" `shouldSatisfy` isLeft'

  describe "list patterns" $ do
    -- BUG (currently fails): PatternCons inspects the rhs pattern's type
    -- instead of unifying it, so `h :: t` is rejected
    it "accepts h :: t" $
      typeOf "r" "let r = match [1] with | h :: t -> h | [] -> 0"
        `shouldBe` Right TInt
    it "accepts a literal list pattern" $
      typeOf "r" "let r = match [1; 2] with | [a; b] -> a + b | _ -> 0"
        `shouldBe` Right TInt
    it "rejects a cons pattern against a non-list" $
      infer "let r = match 1 with | h :: t -> h | _ -> 0" `shouldSatisfy` isLeft'
    it "rejects a cons pattern whose head has the wrong type" $
      infer "let r = match [1] with | \"a\" :: t -> 0 | _ -> 1" `shouldSatisfy` isLeft'

  describe "user-defined variant types" $ do
    it "gives a constructor its declared type" $ do
      -- BUG (currently fails): variant types are unbound type vars,
      -- should be TCon "color" []
      typeOf "x" "type nonrec color = Red | Green\nlet x = Red"
        `shouldBe` Right (TCon "color" [])
    it "rejects using a variant as an int" $
      -- BUG (currently fails): `color` is a type var that binds to int
      infer "type nonrec color = Red | Green\nlet x : color = 1" `shouldSatisfy` isLeft'
    it "rejects matching constructors of different types together" $
      -- BUG (currently fails): both types are just type vars
      infer "type nonrec a = A | B\ntype nonrec b = C | D\nlet z = match A with | C -> 1 | D -> 2"
        `shouldSatisfy` isLeft'
    it "accepts matching constructors of one type" $
      typeOf "z" "type nonrec a = A | B\nlet z = match A with | A -> 1 | B -> 2"
        `shouldBe` Right TInt
    it "checks constructor argument types" $ do
      infer "type nonrec t = Wrap of int\nlet x = Wrap \"a\"" `shouldSatisfy` isLeft'
      typeOf "x" "type nonrec t = Wrap of int\nlet x = Wrap 1" `shouldBe` Right (TCon "t" [])

  describe "for loops" $ do
    it "types a to loop as unit" $
      typeOf "r" "let r = for i = 1 to 10 do if i < 2 then () else () done"
        `shouldBe` Right TUnit
    it "types a downto loop as unit" $
      typeOf "r" "let r = for i = 10 downto 1 do if i < 2 then () else () done"
        `shouldBe` Right TUnit
    it "keeps the loop as a TEFor node" $ do
      TProgram defs <- either fail return (infer "let r = for i = 1 to 3 do () done")
      case defs of
        [TDefinitionLet _ [TLetBinding _ (TEFor _ "i" _ _ True _)]] -> return ()
        _ -> expectationFailure $ "expected a TEFor, got " ++ show defs
    it "rejects non-int bounds" $ do
      infer "let r = for i = 1.0 to 3 do () done" `shouldSatisfy` isLeft'
      infer "let r = for i = 1 to \"a\" do () done" `shouldSatisfy` isLeft'
    it "rejects a non-unit body" $
      infer "let r = for i = 1 to 3 do i done" `shouldSatisfy` isLeft'
    it "doesn't leak the loop variable" $
      infer "let r = for i = 1 to 3 do () done\nlet s = i" `shouldSatisfy` isLeft'

  describe "scopes" $ do
    it "doesn't leak let-bound names" $
      infer "let r = (let y = 1 in y)\nlet s = y" `shouldSatisfy` isLeft'
    it "doesn't leak match-bound names" $
      infer "let r = match 1 with | z -> z\nlet s = z" `shouldSatisfy` isLeft'

  describe "type aliases" $ do
    it "treats an alias as its underlying type" $
      typeOf "x" "type nonrec t = int\nlet x : t = 1" `shouldBe` Right TInt
    it "rejects a value that doesn't match the aliased type" $ do
      infer "type nonrec t = int\nlet x : t = \"a\"" `shouldSatisfy` isLeft'
      infer "type nonrec t = int\nlet x : t = 1.0" `shouldSatisfy` isLeft'
    it "lets an alias be used interchangeably with its type" $
      typeOf "y" "type nonrec t = int\nlet x : t = 1\nlet y = x + 1" `shouldBe` Right TInt
    it "supports aliases of tuple types" $
      typeOf "x" "type nonrec p = int * string\nlet x : p = (1, \"a\")"
        `shouldBe` Right (TTuple [TInt, TString])
    it "supports aliases of variant types" $
      typeOf "x" "type nonrec c = Red | Green\ntype nonrec d = c\nlet x : d = Red"
        `shouldBe` Right (TCon "c" [])
    it "supports aliases of aliases" $
      typeOf "x" "type nonrec a = int\ntype nonrec b = a\nlet x : b = 1"
        `shouldBe` Right TInt
    it "doesn't let a variant alias accept other types" $
      infer "type nonrec c = Red | Green\ntype nonrec d = c\nlet x : d = 1"
        `shouldSatisfy` isLeft'

  describe "variables bound twice in one pattern" $ do
    let dup = failsWith "multiple times"
    it "rejects a repeated name in a tuple pattern" $ do
      infer "let (a, a) = (1, 2)" `shouldSatisfy` dup
      infer "let r = match (1, 2) with | (a, a) -> a" `shouldSatisfy` dup
    it "rejects a repeated name across a cons pattern" $
      infer "let r = match [1] with | h :: h -> h | _ -> 0" `shouldSatisfy` dup
    it "rejects a repeated name in a list pattern" $
      infer "let r = match [1; 2] with | [a; a] -> a | _ -> 0" `shouldSatisfy` dup
    it "rejects an alias that reuses a name from inside the pattern" $ do
      infer "let r = match (1, 2) with | ((a, b) as a) -> 0" `shouldSatisfy` dup
      infer "let r = match 1 with | (z as z) -> z" `shouldSatisfy` dup
    it "rejects a repeated name inside a constructor argument" $
      infer "type nonrec p = Pair of int * int\nlet r = match Pair (1, 2) with | Pair (a, a) -> a"
        `shouldSatisfy` dup
    it "accepts distinct names" $
      typeOf "r" "let r = match (1, 2) with | (a, b) -> a + b" `shouldBe` Right TInt
    it "accepts the same name in different match cases" $
      typeOf "r" "let r = match 1 with | 0 -> 0 | x -> x" `shouldBe` Right TInt
    it "accepts repeated wildcards" $
      typeOf "r" "let r = match (1, 2, 3) with | (_, _, c) -> c" `shouldBe` Right TInt

  describe "shadowing" $ do
    -- shadowing something from an outer scope is not "bound twice in this pattern"
    it "lets a pattern variable shadow an earlier let" $
      typeOf "r" "let x = 1\nlet r = match \"s\" with | x -> x" `shouldBe` Right TString
    it "lets a pattern variable shadow a builtin" $
      typeOf "r" "let (print_endline, b) = (1, 2)\nlet r = print_endline + b"
        `shouldBe` Right TInt
    it "lets a later top-level pattern rebind an earlier name" $
      typeOf "r" "let (a, b) = (1, 2)\nlet (a, c) = (\"s\", 4)\nlet r = a" `shouldBe` Right TString
    it "lets a match shadow a variable from an enclosing match" $
      typeOf "r" "let r = match 1 with | x -> (match \"s\" with | x -> x)" `shouldBe` Right TString

  describe "sequences" $ do
    it "types a sequence as its right-hand side" $
      typeOf "r" "let r = (); 1" `shouldBe` Right TInt
    it "rejects a non-unit left-hand side" $
      infer "let r = 1; 2" `shouldSatisfy` isLeft'
