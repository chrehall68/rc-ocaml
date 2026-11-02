module PatternSpec (spec) where

import Ast
import Lexer (alexScanTokens)
import MyParser (parsePattern)
import Test.Hspec
import Tokens

pat :: String -> Either String Pattern
pat source = alexScanTokens source >>= parsePattern

name :: String -> Pattern
name = PatternVariable

int :: Int -> Pattern
int n = PatternConstant (ConstantInt (IntLiteral n Unspecified))

variant :: String -> Pattern
variant n = PatternConstant (ConstantVariant (Path Nothing n))

typeName :: String -> TypeExpr
typeName = TypeName . ExtendedPath Nothing

spec :: Spec
spec = do
  describe "atomic patterns" $ do
    it "parses variables and wildcards" $ do
      pat "x" `shouldBe` Right (name "x")
      pat "_" `shouldBe` Right PatternWildcard
    it "parses literal and signed constants" $ do
      pat "42" `shouldBe` Right (int 42)
      pat "-42" `shouldBe` Right (int (-42))
      pat "-1.5" `shouldBe` Right (PatternConstant (ConstantDouble (-1.5)))
      pat "'a'" `shouldBe` Right (PatternConstant (ConstantChar 'a'))
      pat "\"hi\"" `shouldBe` Right (PatternConstant (ConstantString "hi"))
      pat "true" `shouldBe` Right (PatternConstant (ConstantBool True))
      pat "false" `shouldBe` Right (PatternConstant (ConstantBool False))
      pat "()" `shouldBe` Right (PatternConstant ConstantUnit)
    it "parses constructor paths and polymorphic tags" $ do
      pat "None" `shouldBe` Right (variant "None")
      pat "Option.None"
        `shouldBe` Right (PatternConstant (ConstantVariant (Path (Just (ModulePath ["Option"])) "None")))
      pat "`Red" `shouldBe` Right (PatternConstant (ConstantPolymorphicVariant "Red"))
      pat "#M.t" `shouldBe` Right (PatternType (ExtendedPath (Just (ExtendedModulePath [ExtendedModuleName "M" []])) "t"))
      pat "#A.B.t"
        `shouldBe` Right
          (PatternType (ExtendedPath (Just (ExtendedModulePath [ExtendedModuleName "A" [], ExtendedModuleName "B" []])) "t"))
      pat "#F(M).t"
        `shouldBe` Right
          (PatternType (ExtendedPath
            (Just (ExtendedModulePath [ExtendedModuleName "F" [ExtendedModulePath [ExtendedModuleName "M" []]]])) "t"))
    it "parses an operator binder" $
      pat "(+)" `shouldBe` Right (name "+")
    it "parses character ranges" $
      pat "'a'..'z'" `shouldBe` Right (PatternRange 'a' 'z')

  describe "nested patterns and precedence" $ do
    it "parses constructor applications" $ do
      pat "Some x" `shouldBe` Right (PatternVariant (Path Nothing "Some") (name "x"))
      pat "Some (Some 1)"
        `shouldBe` Right
          (PatternVariant (Path Nothing "Some") (PatternVariant (Path Nothing "Some") (int 1)))
      pat "`Value x" `shouldBe` Right (PatternPolymorphicVariant "Value" (name "x"))
    it "makes :: right associative" $
      pat "x :: y :: xs"
        `shouldBe` Right (PatternCons (name "x") (PatternCons (name "y") (name "xs")))
    it "makes constructor application bind tighter than ::" $
      pat "Some x :: xs"
        `shouldBe` Right (PatternCons (PatternVariant (Path Nothing "Some") (name "x")) (name "xs"))
    it "parses tuples without losing their structure" $ do
      pat "x, y, z" `shouldBe` Right (PatternTuple [name "x", name "y", name "z"])
      pat "(x, y), z" `shouldBe` Right (PatternTuple [PatternTuple [name "x", name "y"], name "z"])
    it "makes :: bind tighter than tuples and |" $
      pat "x :: xs, y | None"
        `shouldBe` Right (PatternOr (PatternTuple [PatternCons (name "x") (name "xs"), name "y"]) (variant "None"))
    it "makes | bind tighter than as, and | associate left" $ do
      pat "A | B | C as choice"
        `shouldBe` Right (PatternAlias (PatternOr (PatternOr (variant "A") (variant "B")) (variant "C")) "choice")
      pat "(A | B) as choice"
        `shouldBe` Right (PatternAlias (PatternOr (variant "A") (variant "B")) "choice")
    it "parses lazy and exception patterns" $ do
      pat "lazy Some x"
        `shouldBe` Right (PatternLazy (PatternVariant (Path Nothing "Some") (name "x")))
      pat "exception Failure msg"
        `shouldBe` Right (PatternException (PatternVariant (Path Nothing "Failure") (name "msg")))

  describe "collections and records" $ do
    it "parses lists and arrays including empty and trailing semicolons" $ do
      pat "[]" `shouldBe` Right (PatternList [])
      pat "[||]" `shouldBe` Right (PatternArray [])
      pat "[x; Some y;]" `shouldBe` Right (PatternList [name "x", PatternVariant (Path Nothing "Some") (name "y")])
      pat "[|x; _;|]" `shouldBe` Right (PatternArray [name "x", PatternWildcard])
    it "parses shorthand and explicitly bound fields" $
      pat "{ x; M.y = Some z }"
        `shouldBe` Right
          (PatternRecord
            [ PatternField (Path Nothing "x") Nothing (name "x"),
              PatternField (Path (Just (ModulePath ["M"])) "y") Nothing
                (PatternVariant (Path Nothing "Some") (name "z"))
            ] False)
    it "supports open records and a final semicolon" $ do
      pat "{x; _}" `shouldBe` Right (PatternRecord [PatternField (Path Nothing "x") Nothing (name "x")] True)
      pat "{x; _;}" `shouldBe` pat "{x; _}"
      pat "{x;}" `shouldBe` Right (PatternRecord [PatternField (Path Nothing "x") Nothing (name "x")] False)
    it "supports field and pattern type constraints" $ do
      pat "(x : int)" `shouldBe` Right (PatternAnnotated (name "x") (typeName "int"))
      pat "(x : ('a, 'b) result)"
        `shouldBe` Right (PatternAnnotated (name "x")
          (TypeApplication [TypeVariable "a", TypeVariable "b"] (ExtendedPath Nothing "result")))
      pat "(x : int * bool -> float -> string)"
        `shouldBe` Right (PatternAnnotated (name "x")
          (TypeArrow
            (TypeTuple [typeName "int", typeName "bool"])
            (TypeArrow (typeName "float") (typeName "string"))))
      pat "{ x : int = value; _ }"
        `shouldBe` Right (PatternRecord
          [PatternField (Path Nothing "x") (Just (typeName "int")) (name "value")] True)
      pat "{M.x : int}"
        `shouldBe` Right (PatternRecord
          [PatternField (Path (Just (ModulePath ["M"])) "x") (Just (typeName "int")) (name "x")] False)
    it "parses local module opens with each container" $ do
      pat "M.(Some x)"
        `shouldBe` Right (PatternLocalOpen (ModulePath ["M"]) (PatternVariant (Path Nothing "Some") (name "x")))
      pat "M.[x]"
        `shouldBe` Right (PatternLocalOpen (ModulePath ["M"]) (PatternList [name "x"]))
      pat "M.[|x|]"
        `shouldBe` Right (PatternLocalOpen (ModulePath ["M"]) (PatternArray [name "x"]))
      pat "M.{x}"
        `shouldBe` Right (PatternLocalOpen (ModulePath ["M"]) (PatternRecord
          [PatternField (Path Nothing "x") Nothing (name "x")] False))

  describe "invalid patterns" $ do
    it "rejects missing constructor arguments and separators" $ do
      pat "Some x y" `shouldSatisfy` isLeft
      pat "[x;; y]" `shouldSatisfy` isLeft
      pat "{x = }" `shouldSatisfy` isLeft
      pat "x as" `shouldSatisfy` isLeft
    it "rejects empty records and incomplete character ranges" $ do
      pat "{}" `shouldSatisfy` isLeft
      pat "'a'.." `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
