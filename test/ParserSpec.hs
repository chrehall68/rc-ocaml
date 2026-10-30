module ParserSpec (spec) where

import Ast
import Lexer (alexScanTokens)
import MyParser (parse)
import Test.Hspec
import Tokens

-- lex and parse a whole source string
parseSource :: String -> Either String Program
parseSource source = alexScanTokens source >>= parse

-- the expressions of a program made only of items from the implementation
exprsOf :: String -> Either String [Expr]
exprsOf source = fmap items (parseSource source)
  where
    items (Program _ (UnitImplementation Nothing)) = []
    items (Program _ (UnitImplementation (Just (ModuleItems is)))) = [e | Expr e <- is]

-- parse a single expression
expr :: String -> Either String Expr
expr source = exprsOf source >>= single
  where
    single [e] = Right e
    single es = Left ("expected exactly one expression, got " ++ show (length es))

var :: String -> Expr
var name = ExprPath (Path Nothing name)

qualifiedVar :: [String] -> String -> Expr
qualifiedVar modules name = ExprPath (Path (Just (ModulePath modules)) name)

variant :: String -> Expr
variant name = ExprConstant (ConstantVariant (Path Nothing name))

int :: Int -> Expr
int v = ExprConstant (ConstantInt (IntLiteral {value = v, representation = Unspecified}))

spec :: Spec
spec = do
  describe "programs" $ do
    it "parses an empty program" $
      parseSource "" `shouldBe` Right (Program UnitInterface (UnitImplementation Nothing))
    it "rejects stray tokens" $
      parseSource ")" `shouldSatisfy` isLeft
    it "rejects two expressions without a ;; between them" $
      exprsOf "a b c" `shouldSatisfy` isLeft

  describe "module items" $ do
    it "separates expressions with ;;" $
      exprsOf "a ;; b ;; c" `shouldBe` Right [var "a", var "b", var "c"]
    it "allows leading, trailing and repeated ;;" $
      exprsOf ";; ;; a ;; ;; ;; b ;;" `shouldBe` Right [var "a", var "b"]
    it "allows definitions in between" $
      exprsOf "a ;; let ;; b" `shouldBe` Right [var "a", var "b"]

  describe "names" $ do
    it "parses unqualified values" $
      expr "x" `shouldBe` Right (var "x")
    it "parses module-qualified values" $
      expr "A.B.x" `shouldBe` Right (qualifiedVar ["A", "B"] "x")
    it "parses operators as values" $ do
      expr "(+)" `shouldBe` Right (var "+")
      expr "(mod)" `shouldBe` Right (var "mod")
      expr "A.(+)" `shouldBe` Right (qualifiedVar ["A"] "+")
    it "parses unqualified constructors" $
      expr "C" `shouldBe` Right (variant "C")
    it "parses qualified constructors" $
      expr "A.B.C"
        `shouldBe` Right (ExprConstant (ConstantVariant (Path (Just (ModulePath ["A", "B"])) "C")))

  describe "constants" $ do
    it "parses literals" $ do
      expr "1" `shouldBe` Right (int 1)
      expr "1.5" `shouldBe` Right (ExprConstant (ConstantDouble 1.5))
      expr "'a'" `shouldBe` Right (ExprConstant (ConstantChar 'a'))
      expr "\"hi\"" `shouldBe` Right (ExprConstant (ConstantString "hi"))
    it "parses booleans" $ do
      expr "true" `shouldBe` Right (ExprConstant (ConstantBool True))
      expr "false" `shouldBe` Right (ExprConstant (ConstantBool False))
    it "parses unit both ways" $ do
      expr "()" `shouldBe` Right (ExprConstant ConstantUnit)
      expr "begin end" `shouldBe` Right (ExprConstant ConstantUnit)
    it "parses empty lists and arrays" $ do
      expr "[]" `shouldBe` Right (ExprConstant ConstantList)
      expr "[||]" `shouldBe` Right (ExprConstant ConstantArray)
    it "parses polymorphic variant tags" $
      expr "`Tag" `shouldBe` Right (ExprConstant (ConstantPolymorphicVariant "Tag"))

  describe "parentheses" $ do
    it "parses parenthesized expressions" $
      expr "(x)" `shouldBe` Right (var "x")
    it "parses begin ... end" $
      expr "begin x end" `shouldBe` Right (var "x")

  describe "constructor application" $ do
    it "applies a constructor to an argument" $
      expr "C 1" `shouldBe` Right (ExprNonconstantVariant (Path Nothing "C") (int 1))
    it "applies a constructor to a constructor application" $
      expr "C D 1"
        `shouldBe` Right
          (ExprNonconstantVariant (Path Nothing "C") (ExprNonconstantVariant (Path Nothing "D") (int 1)))
    it "applies a polymorphic variant tag" $
      expr "`Tag 1" `shouldBe` Right (ExprNonconstantPolymorphicVariant "Tag" (int 1))

  describe "tuples" $ do
    it "parses a flat tuple" $
      expr "a, b, c" `shouldBe` Right (ExprTuple [var "a", var "b", var "c"])
    it "keeps a parenthesized tuple on the left nested" $
      expr "(a, b), c" `shouldBe` Right (ExprTuple [ExprTuple [var "a", var "b"], var "c"])
    it "keeps a parenthesized tuple on the right nested" $
      expr "a, (b, c)" `shouldBe` Right (ExprTuple [var "a", ExprTuple [var "b", var "c"]])
    it "binds constructor application tighter than ," $
      expr "C a, b"
        `shouldBe` Right (ExprTuple [ExprNonconstantVariant (Path Nothing "C") (var "a"), var "b"])
    it "lets parentheses move the comma inside the application" $
      expr "C (a, b)"
        `shouldBe` Right (ExprNonconstantVariant (Path Nothing "C") (ExprTuple [var "a", var "b"]))

  describe "cons" $ do
    it "parses a chain of ::" $
      expr "a :: b :: c" `shouldBe` Right (ExprCons [var "a", var "b", var "c"])
    it "binds :: tighter than ," $
      expr "a :: b, c"
        `shouldBe` Right (ExprTuple [ExprCons [var "a", var "b"], var "c"])
    it "binds constructor application tighter than ::" $
      expr "C a :: b"
        `shouldBe` Right (ExprCons [ExprNonconstantVariant (Path Nothing "C") (var "a"), var "b"])

  describe "lists and arrays" $ do
    it "parses list literals as a cons chain ending in []" $
      expr "[a; b]" `shouldBe` Right (ExprCons [var "a", var "b", ExprConstant ConstantList])
    it "allows a trailing ; in list literals" $
      expr "[a; b;]" `shouldBe` expr "[a; b]"
    it "parses array literals" $
      expr "[|a; b|]" `shouldBe` Right (ExprArray [var "a", var "b"])

  describe "type annotations" $
    it "has no type expressions yet" $
      expr "(x : t)" `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
