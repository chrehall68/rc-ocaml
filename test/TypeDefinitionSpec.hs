module TypeDefinitionSpec (spec) where

import Ast
import Data.Either (isLeft)
import Lexer (alexScanTokens)
import MyParser (parse)
import Test.Hspec

-- parse a source made of a single type definition
typeDef :: String -> Either String TypeDefinition
typeDef src = alexScanTokens src >>= parse >>= single
  where
    single (Program _ (UnitImplementation (Just (ModuleItems [Definition (DefinitionType d)])))) = Right d
    single p = Left ("expected exactly one type definition, got " ++ show p)

-- the items of a whole program
itemsOf :: String -> Either String [DefinitionOrExpr]
itemsOf src = alexScanTokens src >>= parse >>= items
  where
    items (Program _ (UnitImplementation Nothing)) = Right []
    items (Program _ (UnitImplementation (Just (ModuleItems is)))) = Right is

-- a single, non-recursive-flagged definition with no parameters or constraints
simple :: String -> Maybe TypeExpr -> Maybe TypeRepresentation -> TypeDefinition
simple n eq repr = TypeDefinition False [TypeDef [] n eq repr []]

tname :: String -> TypeExpr
tname = TypeName . ExtendedPath Nothing

tvar :: String -> TypeExpr
tvar = TypeVariable

cdecl :: String -> [TypeExpr] -> ConstrDecl
cdecl = ConstrDecl

spec :: Spec
spec = do
  describe "abstract types and abbreviations" $ do
    it "parses an abstract type" $
      typeDef "type t" `shouldBe` Right (simple "t" Nothing Nothing)
    it "parses a type abbreviation" $
      typeDef "type t = int" `shouldBe` Right (simple "t" (Just (tname "int")) Nothing)
    it "parses abbreviations of arrow, tuple, and applied types" $ do
      typeDef "type f = int -> int"
        `shouldBe` Right (simple "f" (Just (TypeArrow (tname "int") (tname "int"))) Nothing)
      typeDef "type p = int * string"
        `shouldBe` Right (simple "p" (Just (TypeTuple [tname "int", tname "string"])) Nothing)
      typeDef "type l = int list"
        `shouldBe` Right (simple "l" (Just (TypeApplication [tname "int"] (ExtendedPath Nothing "list"))) Nothing)
    it "parses an abbreviation of a module-qualified type" $
      typeDef "type t = M.t"
        `shouldBe` Right (simple "t" (Just (TypeName (ExtendedPath (Just (ExtendedModulePath [ExtendedModuleName "M" []])) "t"))) Nothing)

  describe "type parameters" $ do
    it "parses a single parameter" $
      typeDef "type 'a t = 'a list"
        `shouldBe` Right (TypeDefinition False
          [TypeDef ["a"] "t" (Just (TypeApplication [tvar "a"] (ExtendedPath Nothing "list"))) Nothing []])
    it "parses several parenthesized parameters" $
      typeDef "type ('a, 'b) pair = 'a * 'b"
        `shouldBe` Right (TypeDefinition False
          [TypeDef ["a", "b"] "pair" (Just (TypeTuple [tvar "a", tvar "b"])) Nothing []])
    it "parses a parameter on an abstract type" $
      typeDef "type 'a t" `shouldBe` Right (TypeDefinition False [TypeDef ["a"] "t" Nothing Nothing []])
    it "rejects unsupported variance annotations" $ do
      typeDef "type +'a t" `shouldSatisfy` isLeft
      typeDef "type -'a t" `shouldSatisfy` isLeft
    it "rejects malformed parameter lists" $ do
      typeDef "type () t" `shouldSatisfy` isLeft
      typeDef "type ('a,) t" `shouldSatisfy` isLeft
      typeDef "type 'a 'b t" `shouldSatisfy` isLeft

  describe "variant types" $ do
    it "parses constant constructors" $
      typeDef "type color = Red | Green | Blue"
        `shouldBe` Right (simple "color" Nothing (Just (TypeVariant [cdecl "Red" [], cdecl "Green" [], cdecl "Blue" []])))
    it "allows a leading |" $
      typeDef "type t = | A | B"
        `shouldBe` Right (simple "t" Nothing (Just (TypeVariant [cdecl "A" [], cdecl "B" []])))
    it "parses a single constructor" $ do
      typeDef "type t = A" `shouldBe` Right (simple "t" Nothing (Just (TypeVariant [cdecl "A" []])))
      typeDef "type t = | A" `shouldBe` Right (simple "t" Nothing (Just (TypeVariant [cdecl "A" []])))
    it "parses the empty variant type" $
      typeDef "type t = |" `shouldBe` Right (simple "t" Nothing (Just (TypeVariant [])))
    it "parses constructor arguments" $
      typeDef "type 'a tree = Lf | Br of 'a * 'a tree * 'a"
        `shouldBe` Right (TypeDefinition False
          [ TypeDef ["a"] "tree" Nothing
              (Just (TypeVariant
                [ cdecl "Lf" [],
                  cdecl "Br" [tvar "a", TypeApplication [tvar "a"] (ExtendedPath Nothing "tree"), tvar "a"]
                ]))
              []
          ])
    it "distinguishes several arguments from one tuple argument" $ do
      typeDef "type t = A of int * int"
        `shouldBe` Right (simple "t" Nothing (Just (TypeVariant [cdecl "A" [tname "int", tname "int"]])))
      typeDef "type t = A of (int * int)"
        `shouldBe` Right (simple "t" Nothing (Just (TypeVariant [cdecl "A" [TypeTuple [tname "int", tname "int"]]])))
    it "requires arrow arguments to be parenthesized" $ do
      typeDef "type t = A of (int -> int)"
        `shouldBe` Right (simple "t" Nothing (Just (TypeVariant [cdecl "A" [TypeArrow (tname "int") (tname "int")]])))
      typeDef "type t = A of int -> int" `shouldSatisfy` isLeft
    it "parses [] and (::) as constructor names" $
      typeDef "type 'a l = [] | (::) of 'a * 'a l"
        `shouldBe` Right (TypeDefinition False
          [ TypeDef ["a"] "l" Nothing
              (Just (TypeVariant
                [ cdecl "[]" [],
                  cdecl "::" [tvar "a", TypeApplication [tvar "a"] (ExtendedPath Nothing "l")]
                ]))
              []
          ])
    it "rejects malformed variants" $ do
      typeDef "type t = A |" `shouldSatisfy` isLeft
      typeDef "type t = A | | B" `shouldSatisfy` isLeft
      typeDef "type t = A of" `shouldSatisfy` isLeft
      typeDef "type t = a of int" `shouldSatisfy` isLeft

  describe "record types" $ do
    it "parses fields" $
      typeDef "type point = { x : int; y : int }"
        `shouldBe` Right (simple "point" Nothing (Just (TypeRecord [FieldDecl False "x" (tname "int"), FieldDecl False "y" (tname "int")])))
    it "allows a trailing ;" $
      typeDef "type point = { x : int; y : int; }"
        `shouldBe` Right (simple "point" Nothing (Just (TypeRecord [FieldDecl False "x" (tname "int"), FieldDecl False "y" (tname "int")])))
    it "parses mutable fields" $
      typeDef "type r = { mutable count : int; name : string }"
        `shouldBe` Right (simple "r" Nothing (Just (TypeRecord [FieldDecl True "count" (tname "int"), FieldDecl False "name" (tname "string")])))
    it "parses fields with arrow and tuple types" $
      typeDef "type r = { f : int -> int * int }"
        `shouldBe` Right (simple "r" Nothing (Just (TypeRecord [FieldDecl False "f" (TypeArrow (tname "int") (TypeTuple [tname "int", tname "int"]))])))
    it "rejects malformed records" $ do
      typeDef "type r = { }" `shouldSatisfy` isLeft
      typeDef "type r = { x : int;; }" `shouldSatisfy` isLeft
      typeDef "type r = { x }" `shouldSatisfy` isLeft
      typeDef "type r = { X : int }" `shouldSatisfy` isLeft

  describe "re-exported representations" $ do
    it "parses an equation followed by a variant" $
      typeDef "type t = M.t = A | B"
        `shouldBe` Right (simple "t"
          (Just (TypeName (ExtendedPath (Just (ExtendedModulePath [ExtendedModuleName "M" []])) "t")))
          (Just (TypeVariant [cdecl "A" [], cdecl "B" []])))
    it "parses an equation followed by a record" $
      typeDef "type t = u = { x : int }"
        `shouldBe` Right (simple "t" (Just (tname "u")) (Just (TypeRecord [FieldDecl False "x" (tname "int")])))

  describe "constraints" $ do
    it "parses a constraint" $
      typeDef "type 'a t = 'a list constraint 'a = int"
        `shouldBe` Right (TypeDefinition False
          [ TypeDef ["a"] "t" (Just (TypeApplication [tvar "a"] (ExtendedPath Nothing "list"))) Nothing
              [TypeConstraint (tvar "a") (tname "int")]
          ])
    it "parses several constraints, including on abstract types and representations" $ do
      typeDef "type ('a, 'b) t constraint 'a = int constraint 'b = 'a list"
        `shouldBe` Right (TypeDefinition False
          [ TypeDef ["a", "b"] "t" Nothing Nothing
              [ TypeConstraint (tvar "a") (tname "int"),
                TypeConstraint (tvar "b") (TypeApplication [tvar "a"] (ExtendedPath Nothing "list"))
              ]
          ])
      typeDef "type 'a t = A of 'a constraint 'a = int"
        `shouldBe` Right (TypeDefinition False
          [TypeDef ["a"] "t" Nothing (Just (TypeVariant [cdecl "A" [tvar "a"]])) [TypeConstraint (tvar "a") (tname "int")]])
    it "rejects incomplete constraints" $ do
      typeDef "type 'a t constraint 'a" `shouldSatisfy` isLeft
      typeDef "type 'a t constraint 'a =" `shouldSatisfy` isLeft

  describe "multiple and recursive definitions" $ do
    it "parses the manual's mutually recursive example" $
      typeDef "type t = { decoration : string; substance : t' } and t' = Int of int | List of t list"
        `shouldBe` Right (TypeDefinition False
          [ TypeDef [] "t" Nothing
              (Just (TypeRecord [FieldDecl False "decoration" (tname "string"), FieldDecl False "substance" (tname "t'")]))
              [],
            TypeDef [] "t'" Nothing
              (Just (TypeVariant [cdecl "Int" [tname "int"], cdecl "List" [TypeApplication [tname "t"] (ExtendedPath Nothing "list")]]))
              []
          ])
    it "parses definitions joined by and" $
      typeDef "type t = { left : u } and u = Leaf | Node of t"
        `shouldBe` Right (TypeDefinition False
          [ TypeDef [] "t" Nothing (Just (TypeRecord [FieldDecl False "left" (tname "u")])) [],
            TypeDef [] "u" Nothing (Just (TypeVariant [cdecl "Leaf" [], cdecl "Node" [tname "t"]])) []
          ])
    it "parses nonrec" $
      typeDef "type nonrec t = t list"
        `shouldBe` Right (TypeDefinition True [TypeDef [] "t" (Just (TypeApplication [tname "t"] (ExtendedPath Nothing "list"))) Nothing []])
    it "rejects malformed definitions" $ do
      typeDef "type" `shouldSatisfy` isLeft
      typeDef "type = int" `shouldSatisfy` isLeft
      typeDef "type T = int" `shouldSatisfy` isLeft
      typeDef "type t =" `shouldSatisfy` isLeft
      typeDef "type t = int and" `shouldSatisfy` isLeft
      typeDef "type rec t = int" `shouldSatisfy` isLeft

  describe "type definitions as module items" $ do
    it "allows consecutive definitions without ;;" $
      itemsOf "type t = int type u = t"
        `shouldBe` Right
          [ Definition (DefinitionType (simple "t" (Just (tname "int")) Nothing)),
            Definition (DefinitionType (simple "u" (Just (tname "t")) Nothing))
          ]
    it "mixes definitions and expressions" $
      itemsOf "type t = A ;; x ;; type u = B"
        `shouldBe` Right
          [ Definition (DefinitionType (simple "t" Nothing (Just (TypeVariant [cdecl "A" []])))),
            Expr (ExprPath (Path Nothing "x")),
            Definition (DefinitionType (simple "u" Nothing (Just (TypeVariant [cdecl "B" []]))))
          ]
    it "allows a definition directly after an expression" $
      itemsOf "x type t = int"
        `shouldBe` Right
          [ Expr (ExprPath (Path Nothing "x")),
            Definition (DefinitionType (simple "t" (Just (tname "int")) Nothing))
          ]
    it "allows an expression after a definition when separated by ;;" $
      itemsOf "type t = int ;; x"
        `shouldBe` Right
          [ Definition (DefinitionType (simple "t" (Just (tname "int")) Nothing)),
            Expr (ExprPath (Path Nothing "x"))
          ]
