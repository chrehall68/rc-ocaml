module TypeAnnotatorSpec (spec) where

import Data.List (isInfixOf)
import Frontend.Ast (Path (..))
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

  describe "function definitions" $ do
    it "types a one-argument function" $
      typeOf "f" "let f x = x + 1" `shouldBe` Right (TArrow NoLabel TInt TInt)
    it "types a curried function" $
      typeOf "f" "let f x (y : string) = x + 1"
        `shouldBe` Right (TArrow NoLabel TInt (TArrow NoLabel TString TInt))
    it "types a pattern parameter" $
      typeOf "f" "let f (a, b) = a + b"
        `shouldBe` Right (TArrow NoLabel (TTuple [TInt, TInt]) TInt)
    it "types a function with a return annotation" $
      typeOf "f" "let f x : int = x" `shouldBe` Right (TArrow NoLabel TInt TInt)
    it "rejects a wrong return annotation" $
      infer "let f x : string = x + 1" `shouldSatisfy` isLeft'
    it "types a lambda" $
      typeOf "f" "let f = fun x -> x + 1" `shouldBe` Right (TArrow NoLabel TInt TInt)
    it "types a labeled parameter" $
      typeOf "f" "let f ~x = x + 1" `shouldBe` Right (TArrow (Labeled "x") TInt TInt)
    it "types an optional parameter with a default" $
      typeOf "f" "let f ?(x = 1) () = x"
        `shouldBe` Right (TArrow (Optional "x") (TCon "option" [TInt]) (TArrow NoLabel TUnit TInt))
    it "applies a defined function" $
      typeOf "r" "let f x y = x + y\nlet r = f 1 2" `shouldBe` Right TInt
    it "applies a labeled function out of order" $
      typeOf "r" "let f ~a ~b = a - b\nlet r = f ~b:1 ~a:2" `shouldBe` Right TInt
    it "keeps unlabeled args in order around a label" $ do
      typeOf "r" "let f ~a x (y : string) = x + a\nlet r = f 1 ~a:2 \"s\"" `shouldBe` Right TInt
      infer "let f ~a x (y : string) = x + a\nlet r = f \"s\" ~a:2 1" `shouldSatisfy` isLeft'
    it "eta-expands a skipped labeled parameter" $
      typeOf "r" "let f ~a x = x + a\nlet r = f 1"
        `shouldBe` Right (TArrow (Labeled "a") TInt TInt)
    it "defaults an omitted optional before a positional" $
      typeOf "r" "let f ?(x = 1) () = x\nlet r = f ()" `shouldBe` Right TInt
    it "rejects wrong argument types" $
      infer "let f x = x + 1\nlet r = f \"a\"" `shouldSatisfy` isLeft'

  describe "let-polymorphism" $ do
    it "lets an identity function be used at two types" $
      typeOf "r" "let id x = x\nlet a = id 1\nlet r = (a, id \"s\")"
        `shouldBe` Right (TTuple [TInt, TString])
    it "gives a polymorphic function distinct type variables per use" $
      typeOf "r" "let pair x = (x, x)\nlet r = (pair 1, pair true)"
        `shouldBe` Right (TTuple [TTuple [TInt, TInt], TTuple [TBool, TBool]])
    it "keeps lambda parameters monomorphic" $
      infer "let h f = (f 1, f true)" `shouldSatisfy` isLeft'
    it "generalizes local functions" $
      typeOf "r" "let r = let id x = x in (id 1, id true)"
        `shouldBe` Right (TTuple [TInt, TBool])
    it "does not generalize variables of an enclosing parameter" $ do
      -- g : 'b -> 'a * 'b, but 'a belongs to x
      typeOf "r" "let f x = let g y = (x, y) in (g 1, g true)\nlet r = f 5"
        `shouldBe` Right (TTuple [TTuple [TInt, TInt], TTuple [TInt, TBool]])
      -- x can't be both an int and a string through g
      infer "let f x = let g y = x in (g 1) + 1 + (if g true then 1 else 2)" `shouldSatisfy` isLeft'
    it "applies a polymorphic builtin comparison at several types" $
      typeOf "r" "let r = (1 < 2, \"a\" < \"b\")" `shouldBe` Right (TTuple [TBool, TBool])
    it "does not generalize an expansive right-hand side" $ do
      -- id id is an application, so f stays weakly typed
      typeOf "r" "let id x = x\nlet f = id id\nlet r = f 1" `shouldBe` Right TInt
      infer "let id x = x\nlet f = id id\nlet a = f 1\nlet b = f \"s\"" `shouldSatisfy` isLeft'

  describe "let-polymorphism with patterns" $ do
    it "generalizes variables bound by a pattern of values" $
      typeOf "r" "let (f, g) = ((fun x -> x), (fun y -> y))\nlet r = (f 1, f true, g \"s\")"
        `shouldBe` Right (TTuple [TInt, TBool, TString])
    it "does not generalize a pattern whose right-hand side is expansive" $ do
      let src = "let id x = x\nlet (a, b) = (id id, 2)\n"
      typeOf "r" (src ++ "let r = a 1") `shouldBe` Right TInt
      infer (src ++ "let x = a 1\nlet y = a true") `shouldSatisfy` isLeft'
    it "does not generalize variables an enclosing parameter depends on" $
      typeOf "r" "let f x = let (a, b) = (x, 1) in (a, b)\nlet r = f true"
        `shouldBe` Right (TTuple [TBool, TInt])
    it "keeps pattern variables tied to a parameter's type" $
      infer "let f x = let (a, _) = (x, 1) in (a + 1, if a then 1 else 2)" `shouldSatisfy` isLeft'

  describe "polymorphic types" $ do
    it "types the builtin option constructors" $ do
      typeOf "r" "let r = Some 1" `shouldBe` Right (TCon "option" [TInt])
      typeOf "r" "let r = Some \"s\"" `shouldBe` Right (TCon "option" [TString])
    it "lets None be used at any type" $
      typeOf "r" "let a : int option = None\nlet b : string option = None\nlet r = (a, b)"
        `shouldBe` Right (TTuple [TCon "option" [TInt], TCon "option" [TString]])
    it "unifies None with the other branch" $
      typeOf "r" "let r = if true then None else Some 1" `shouldBe` Right (TCon "option" [TInt])
    it "matches on option" $
      typeOf "r" "let r = match Some 1 with | Some x -> x + 1 | None -> 0" `shouldBe` Right TInt
    it "rejects an option of the wrong type" $
      infer "let r : string option = Some 1" `shouldSatisfy` isLeft'
    it "defines a polymorphic variant type" $ do
      let src = "type nonrec 'a box = Box of 'a\n"
      typeOf "r" (src ++ "let r = Box 1") `shouldBe` Right (TCon "box" [TInt])
      typeOf "r" (src ++ "let r = (Box 1, Box true)")
        `shouldBe` Right (TTuple [TCon "box" [TInt], TCon "box" [TBool]])
      typeOf "r" (src ++ "let r = match Box 1 with | Box x -> x") `shouldBe` Right TInt
    it "defines a type with several parameters" $
      typeOf "r" "type nonrec ('a, 'b) either = Left of 'a | Right of 'b\nlet r : (int, string) either = Left 1"
        `shouldBe` Right (TCon "either" [TInt, TString])
    it "defines a polymorphic alias" $
      typeOf "r" "type nonrec 'a pair = 'a * 'a\nlet r : int pair = (1, 2)"
        `shouldBe` Right (TTuple [TInt, TInt])
    it "rejects the wrong number of type arguments" $ do
      infer "let r : option = None" `shouldSatisfy` isLeft'
      infer "let r : (int, int) option = None" `shouldSatisfy` isLeft'
      infer "let r : int int = 1" `shouldSatisfy` isLeft'
    it "rejects an undeclared type parameter" $
      infer "type nonrec 'a t = T of 'b" `shouldSatisfy` isLeft'
    it "uses named type variables in annotations" $
      typeOf "r" "let id (x : 'a) : 'a = x\nlet r = (id 1, id true)" `shouldBe` Right (TTuple [TInt, TBool])
    it "shares a named type variable within one definition" $
      infer "let f (x : 'a) (y : 'a) = (x + 1, y)\nlet r = f 1 \"s\"" `shouldSatisfy` isLeft'

  describe "lists as constructors" $ do
    it "lowers [] to the constant constructor" $ do
      TProgram defs <- either fail return (infer "let r = []")
      case defs of
        [TDefinitionLet _ [TLetBinding _ (TEConstant _ c)]] ->
          c `shouldBe` TConstVariant (Path Nothing "[]")
        _ -> expectationFailure "unexpected shape"
    it "lowers :: to a variant applied to a tuple" $ do
      TProgram defs <- either fail return (infer "let r = 1 :: []")
      case defs of
        [TDefinitionLet _ [TLetBinding _ (TEVariant _ path (TETuple {}))]] -> path `shouldBe` Path Nothing "::"
        _ -> expectationFailure "unexpected shape"
    it "types list literals" $
      typeOf "r" "let r = [1; 2; 3]" `shouldBe` Right (TCon "list" [TInt])
    it "lets [] be a list of anything" $
      typeOf "r" "let r = ([] : int list), ([] : string list)"
        `shouldBe` Right (TTuple [TCon "list" [TInt], TCon "list" [TString]])
    it "rejects mixed list elements" $
      infer "let r = [1; \"a\"]" `shouldSatisfy` isLeft'

  describe "recursive types" $ do
    it "defines a recursive polymorphic type" $ do
      let src = "type 'a mylist = Nil | Cons of 'a * 'a mylist\n"
      typeOf "r" (src ++ "let r = Cons (1, Cons (2, Nil))") `shouldBe` Right (TCon "mylist" [TInt])
      infer (src ++ "let r = Cons (1, Cons (\"a\", Nil))") `shouldSatisfy` isLeft'
    it "defines mutually recursive types" $ do
      let src = "type tree = Node of int * forest and forest = Empty | More of tree * forest\n"
      typeOf "r" (src ++ "let r = Node (1, More (Node (2, Empty), Empty))") `shouldBe` Right (TCon "tree" [])
    it "lets a variant refer to an alias in its group" $
      typeOf "r" "type t = A of u and u = int\nlet r = A 1" `shouldBe` Right (TCon "t" [])
    it "does not let nonrec types refer to themselves" $
      infer "type nonrec t = A of t" `shouldSatisfy` isLeft'
    it "rejects wrong arguments to a recursive type" $
      infer "type 'a t = Leaf | Node of 'a t * 'a * 'a t\nlet r : int t = Node (Leaf, \"s\", Leaf)"
        `shouldSatisfy` isLeft'

  describe "let rec and and" $ do
    it "types a recursive function" $
      typeOf "fact" "let rec fact n = if n < 1 then 1 else n * fact (n - 1)"
        `shouldBe` Right (TArrow NoLabel TInt TInt)
    it "generalizes a recursive function" $
      typeOf "r" "let rec len l = match l with | [] -> 0 | _ :: t -> 1 + len t\nlet r = (len [1], len [true])"
        `shouldBe` Right (TTuple [TInt, TInt])
    it "types mutually recursive functions" $
      typeOf "r" "let rec even n = if n < 1 then true else odd (n - 1) and odd n = if n < 1 then false else even (n - 1)\nlet r = even 4"
        `shouldBe` Right TBool
    it "keeps a recursive function monomorphic inside its own body" $
      infer "let rec f x = (f 1, f true)" `shouldSatisfy` isLeft'
    it "rejects a non-variable pattern in let rec" $
      infer "let rec (a, b) = (1, 2)" `shouldSatisfy` isLeft'
    it "evaluates all right-hand sides outside the new names for let ... and" $ do
      typeOf "r" "let a = 1\nlet a = \"s\" and b = a\nlet r = b" `shouldBe` Right TInt
    it "rejects binding a name twice in one let ... and" $
      infer "let a = 1 and a = 2" `shouldSatisfy` isLeft'
    it "supports local let rec" $
      typeOf "r" "let r = let rec f n = if n < 1 then 0 else f (n - 1) in f 3" `shouldBe` Right TInt

  describe "prefix operators" $ do
    it "types unary minus as a unary function, not the infix operator" $ do
      typeOf "f" "let f x = -x" `shouldBe` Right (TArrow NoLabel TInt TInt)
      typeOf "x" "let f x = -x\nlet x = f 3" `shouldBe` Right TInt
    it "types unary plus" $ do
      typeOf "f" "let f x = +x" `shouldBe` Right (TArrow NoLabel TInt TInt)
      typeOf "f" "let f x = +. x" `shouldBe` Right (TArrow NoLabel TFloat TFloat)
    it "types unary float minus" $
      typeOf "f" "let f x = -. x" `shouldBe` Right (TArrow NoLabel TFloat TFloat)
    it "still types binary minus" $
      typeOf "r" "let r = 3 - 1" `shouldBe` Right TInt
    it "rejects negating a non-number" $
      infer "let r = -\"a\"" `shouldSatisfy` isLeft'

  describe "records" $ do
    let point = "type point = {x : int; y : int}\n"
        box = "type 'a box = {mutable contents : 'a}\n"
    it "types a record literal as its declared type" $
      typeOf "p" (point ++ "let p = {x = 1; y = 2}") `shouldBe` Right (TCon "point" [])
    it "does not care what order the fields are written in" $
      typeOf "p" (point ++ "let p = {y = 2; x = 1}") `shouldBe` Right (TCon "point" [])
    it "types field access" $
      typeOf "a" (point ++ "let p = {x = 1; y = 2}\nlet a = p.x") `shouldBe` Right TInt
    it "infers a function's argument from the field it accesses" $
      typeOf "getx" (point ++ "let getx r = r.x")
        `shouldBe` Right (TArrow NoLabel (TCon "point" []) TInt)
    it "types a field punned from a variable" $
      typeOf "p" (point ++ "let x = 1\nlet y = 2\nlet p = {x; y}") `shouldBe` Right (TCon "point" [])
    it "accepts a field annotation" $
      typeOf "p" (point ++ "let p = {x : int = 1; y = 2}") `shouldBe` Right (TCon "point" [])
    it "rejects a field of the wrong type" $
      infer (point ++ "let p = {x = \"a\"; y = 2}") `shouldSatisfy` isLeft'
    it "rejects a wrong field annotation" $
      infer (point ++ "let p = {x : string = 1; y = 2}") `shouldSatisfy` isLeft'
    it "rejects a missing field" $
      infer (point ++ "let p = {x = 1}") `shouldSatisfy` failsWith "missing"
    it "rejects a repeated field" $
      infer (point ++ "let p = {x = 1; x = 2; y = 3}") `shouldSatisfy` failsWith "more than once"
    it "rejects an unknown field" $
      infer (point ++ "let p = {x = 1; z = 2}") `shouldSatisfy` failsWith "field"
    it "rejects accessing an unknown field" $
      infer (point ++ "let a = {x = 1; y = 2}.z") `shouldSatisfy` isLeft'
    it "rejects accessing a field of the wrong record" $
      infer (point ++ "type other = {name : string}\nlet a = ({name = \"n\"}).x") `shouldSatisfy` isLeft'
    it "rejects mixing fields of different records" $
      infer (point ++ "type other = {name : string}\nlet p = {x = 1; name = \"n\"}")
        `shouldSatisfy` failsWith "same record"
    it "rejects a duplicated field in a type definition" $
      infer "type t = {a : int; a : string}" `shouldSatisfy` failsWith "more than once"
    it "lets the later of two records with the same field name win" $
      typeOf "r" "type a = {f : int}\ntype b = {f : string}\nlet r = {f = \"s\"}"
        `shouldBe` Right (TCon "b" [])

    describe "polymorphic records" $ do
      it "gives each use of a polymorphic record fresh type variables" $ do
        let src = "type 'a pair = {fst : 'a; snd : 'a}\n"
        typeOf "p" (src ++ "let p = {fst = 1; snd = 2}") `shouldBe` Right (TCon "pair" [TInt])
        typeOf "q" (src ++ "let p = {fst = 1; snd = 2}\nlet q = {fst = \"a\"; snd = \"b\"}")
          `shouldBe` Right (TCon "pair" [TString])
      it "makes the fields of one record literal share a type" $
        infer "type 'a pair = {fst : 'a; snd : 'a}\nlet p = {fst = 1; snd = \"a\"}" `shouldSatisfy` isLeft'
      it "types field access through the type parameter" $
        typeOf "a" "type 'a pair = {fst : 'a; snd : 'a}\nlet a = ({fst = 1; snd = 2}).fst" `shouldBe` Right TInt
      it "generalizes a polymorphic field accessor" $
        typeOf "r" "type 'a pair = {fst : 'a; snd : 'a}\nlet get p = p.fst\nlet r = (get {fst = 1; snd = 2}, get {fst = \"a\"; snd = \"b\"})"
          `shouldBe` Right (TTuple [TInt, TString])
      it "allows a field to be a function" $
        typeOf "r" "type t = {f : int -> int}\nlet t = {f = fun x -> x + 1}\nlet r = t.f 3" `shouldBe` Right TInt

    describe "field access in expressions" $ do
      it "works as a function argument" $
        typeOf "r" (point ++ "let p = {x = 1; y = 2}\nlet r = string_of_int p.x") `shouldBe` Right TString
      it "works in an infix expression" $
        typeOf "r" (point ++ "let p = {x = 1; y = 2}\nlet r = p.x + p.y") `shouldBe` Right TInt
      it "chains through nested records" $
        typeOf "r" (point ++ "type line = {a : point}\nlet r = ({a = {x = 1; y = 2}}).a.x") `shouldBe` Right TInt

    describe "recursive records" $ do
      it "lets a record refer to itself" $
        typeOf "n" "type node = {value : int; next : node option}\nlet n = {value = 1; next = Some {value = 2; next = None}}"
          `shouldBe` Right (TCon "node" [])
      it "lets mutually recursive types use records" $
        typeOf "r" "type a = {b : b option} and b = {a : a option}\nlet r = {b = Some {a = None}}"
          `shouldBe` Right (TCon "a" [])

    describe "with" $ do
      it "has the type of the source record" $
        typeOf "q" (point ++ "let p = {x = 1; y = 2}\nlet q = {p with x = 5}") `shouldBe` Right (TCon "point" [])
      it "does not need every field" $
        typeOf "q" (point ++ "let p = {x = 1; y = 2}\nlet q = {p with y = 5}") `shouldBe` Right (TCon "point" [])
      it "rejects a source of a different type" $
        infer (point ++ "type other = {x : int; z : int}\nlet o = {x = 1; z = 2}\nlet q : point = {o with y = 5}")
          `shouldSatisfy` isLeft'
      it "rejects a wrongly typed new field" $
        infer (point ++ "let p = {x = 1; y = 2}\nlet q = {p with x = \"a\"}") `shouldSatisfy` isLeft'
      it "rejects a source that is not a record" $
        infer (point ++ "let q = {1 with x = 5}") `shouldSatisfy` isLeft'

    describe "mutable fields" $ do
      it "types assignment as unit" $
        typeOf "u" (box ++ "let b = {contents = 1}\nlet u = b.contents <- 2") `shouldBe` Right TUnit
      it "rejects assigning an immutable field" $
        infer (point ++ "let p = {x = 1; y = 2}\nlet u = p.x <- 3") `shouldSatisfy` failsWith "not mutable"
      it "rejects assigning the wrong type" $
        infer (box ++ "let b = {contents = 1}\nlet u = b.contents <- \"a\"") `shouldSatisfy` isLeft'
      it "lets assignment be sequenced" $
        typeOf "r" (box ++ "let b = {contents = 1}\nlet r = b.contents <- 2; b.contents") `shouldBe` Right TInt
      -- the value restriction: a record literal might be mutable, so its type
      -- variables stay monomorphic. without this, you could store an int and
      -- read it back out as a string
      it "does not generalize a mutable record, so it cannot be used at two types" $
        infer (box ++ "let b = {contents = []}\nlet u = b.contents <- [1]\nlet v = b.contents <- [\"a\"]")
          `shouldSatisfy` isLeft'
      it "still resolves the weak type variable from a later use" $
        typeOf "b" (box ++ "let b = {contents = []}\nlet u = b.contents <- [1]")
          `shouldBe` Right (TCon "box" [TCon "list" [TInt]])

    describe "record patterns" $ do
      it "binds fields" $
        typeOf "r" (point ++ "let r = match {x = 1; y = 2} with | {x; y} -> x + y") `shouldBe` Right TInt
      it "binds a field to a nested pattern" $
        typeOf "r" (point ++ "let r = match {x = 1; y = 2} with | {x = a; y = 2} -> a | _ -> 0") `shouldBe` Right TInt
      it "allows leaving out fields with ; _" $
        typeOf "r" (point ++ "let r = match {x = 1; y = 2} with | {x; _} -> x") `shouldBe` Right TInt
      it "works in function parameters" $
        typeOf "f" (point ++ "let f {x; y} = x + y")
          `shouldBe` Right (TArrow NoLabel (TCon "point" []) TInt)
      it "works in let patterns" $
        typeOf "r" (point ++ "let {x = a; y = b} = {x = 1; y = 2}\nlet r = a + b") `shouldBe` Right TInt
      it "rejects matching a record against a non-record" $
        infer (point ++ "let r = match 1 with | {x; y} -> x") `shouldSatisfy` isLeft'
      it "rejects a field pattern of the wrong type" $
        infer (point ++ "let r = match {x = 1; y = 2} with | {x = \"a\"; y} -> y") `shouldSatisfy` isLeft'
      it "rejects fields of different records" $
        infer (point ++ "type other = {name : string}\nlet r = match {x = 1; y = 2} with | {x; name} -> x")
          `shouldSatisfy` failsWith "same record"
