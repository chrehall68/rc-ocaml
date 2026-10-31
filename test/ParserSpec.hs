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

infixE :: String -> Expr -> Expr -> Expr
infixE = ExprInfix

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
      exprsOf "C 1 2" `shouldSatisfy` isLeft

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
    it "applies a constructor to a parenthesized constructor application" $
      expr "C (D 1)"
        `shouldBe` Right
          (ExprNonconstantVariant (Path Nothing "C") (ExprNonconstantVariant (Path Nothing "D") (int 1)))
    it "applies a constructor to a nullary constructor" $
      expr "C D" `shouldBe` Right (ExprNonconstantVariant (Path Nothing "C") (variant "D"))
    it "only takes a simple expression as its argument" $
      expr "C f x" `shouldSatisfy` isLeft
    it "applies a polymorphic variant tag" $
      expr "`Tag 1" `shouldBe` Right (ExprNonconstantPolymorphicVariant "Tag" (int 1))

  describe "function application" $ do
    it "applies a function to one argument" $
      expr "f 1" `shouldBe` Right (ExprFunctionApplication (var "f") [UnlabeledArgument (int 1)])
    it "applies a function to several arguments at once" $
      expr "f a b"
        `shouldBe` Right (ExprFunctionApplication (var "f") [UnlabeledArgument (var "a"), UnlabeledArgument (var "b")])
    it "treats a parenthesized application as a single argument" $
      expr "f (g a) b"
        `shouldBe` Right
          ( ExprFunctionApplication
              (var "f")
              [ UnlabeledArgument (ExprFunctionApplication (var "g") [UnlabeledArgument (var "a")]),
                UnlabeledArgument (var "b")
              ]
          )
    it "allows nullary constructors as arguments" $
      expr "f C 1"
        `shouldBe` Right (ExprFunctionApplication (var "f") [UnlabeledArgument (variant "C"), UnlabeledArgument (int 1)])
    it "applies qualified functions" $
      expr "List.map f"
        `shouldBe` Right (ExprFunctionApplication (qualifiedVar ["List"] "map") [UnlabeledArgument (var "f")])
    it "parses labeled and optional arguments" $
      expr "f ~x:1 ~y ?z:2 ?w"
        `shouldBe` Right
          ( ExprFunctionApplication
              (var "f")
              [ LabeledArgument "x" (Just (int 1)),
                LabeledArgument "y" Nothing,
                OptionalArgument "z" (Just (int 2)),
                OptionalArgument "w" Nothing
              ]
          )
    it "binds tighter than ," $
      expr "f a, g b"
        `shouldBe` Right
          ( ExprTuple
              [ ExprFunctionApplication (var "f") [UnlabeledArgument (var "a")],
                ExprFunctionApplication (var "g") [UnlabeledArgument (var "b")]
              ]
          )
    it "binds tighter than ::" $
      expr "f a :: b"
        `shouldBe` Right (ExprCons [ExprFunctionApplication (var "f") [UnlabeledArgument (var "a")], var "b"])
    it "can't be the application of a bare constructor" $
      expr "C 1" `shouldBe` Right (ExprNonconstantVariant (Path Nothing "C") (int 1))

  describe "infix operators" $ do
    let a = var "a"
        b = var "b"
        c = var "c"
    it "parses a single operator from each class" $ do
      expr "a <= b" `shouldBe` Right (infixE "<=" a b)
      expr "a @ b" `shouldBe` Right (infixE "@" a b)
      expr "a +. b" `shouldBe` Right (infixE "+." a b)
      expr "a /. b" `shouldBe` Right (infixE "/." a b)
      expr "a ** b" `shouldBe` Right (infixE "**" a b)
      expr "a #+ b" `shouldBe` Right (infixE "#+" a b)
    it "parses operators that have their own tokens" $ do
      expr "a = b" `shouldBe` Right (infixE "=" a b)
      expr "a != b" `shouldBe` Right (infixE "!=" a b)
      expr "a && b" `shouldBe` Right (infixE "&&" a b)
      expr "a || b" `shouldBe` Right (infixE "||" a b)
      expr "a := b" `shouldBe` Right (infixE ":=" a b)
    it "parses keyword operators" $ do
      expr "a mod b" `shouldBe` Right (infixE "mod" a b)
      expr "a lsl b" `shouldBe` Right (infixE "lsl" a b)
      expr "a or b" `shouldBe` Right (infixE "or" a b)
    it "makes * bind tighter than +" $
      expr "a + b * c" `shouldBe` Right (infixE "+" a (infixE "*" b c))
    it "makes + bind looser than a custom * operator" $
      expr "a *. b +. c" `shouldBe` Right (infixE "+." (infixE "*." a b) c)
    it "treats operators of the same class as one level" $
      expr "a + b -. c" `shouldBe` Right (infixE "-." (infixE "+" a b) c)
    it "treats keyword operators as part of their class" $
      expr "a * b mod c" `shouldBe` Right (infixE "mod" (infixE "*" a b) c)
    it "associates + to the left" $
      expr "a + b + c" `shouldBe` Right (infixE "+" (infixE "+" a b) c)
    it "associates ** to the right" $
      expr "a ** b ** c" `shouldBe` Right (infixE "**" a (infixE "**" b c))
    it "associates @ to the right" $
      expr "a @ b @ c" `shouldBe` Right (infixE "@" a (infixE "@" b c))
    it "associates = to the left" $
      expr "a = b = c" `shouldBe` Right (infixE "=" (infixE "=" a b) c)
    it "associates && to the right" $
      expr "a && b && c" `shouldBe` Right (infixE "&&" a (infixE "&&" b c))
    it "associates := to the right" $
      expr "a := b := c" `shouldBe` Right (infixE ":=" a (infixE ":=" b c))
    it "binds + tighter than ::" $
      expr "a + b :: c" `shouldBe` Right (ExprCons [infixE "+" a b, c])
    it "binds :: tighter than @" $
      expr "a :: b @ c" `shouldBe` Right (infixE "@" (ExprCons [a, b]) c)
    it "binds @ tighter than =" $
      expr "a @ b = c" `shouldBe` Right (infixE "=" (infixE "@" a b) c)
    it "binds = tighter than &&" $
      expr "a = b && c" `shouldBe` Right (infixE "&&" (infixE "=" a b) c)
    it "binds && tighter than ||" $
      expr "a || b && c" `shouldBe` Right (infixE "||" a (infixE "&&" b c))
    it "binds || tighter than ," $
      expr "a || b, c" `shouldBe` Right (ExprTuple [infixE "||" a b, c])
    it "binds , tighter than :=" $
      expr "a := b, c" `shouldBe` Right (infixE ":=" a (ExprTuple [b, c]))
    it "binds function application tighter than operators" $
      expr "f a + g b"
        `shouldBe` Right
          ( infixE
              "+"
              (ExprFunctionApplication (var "f") [UnlabeledArgument a])
              (ExprFunctionApplication (var "g") [UnlabeledArgument b])
          )
    it "binds constructor application tighter than operators" $
      expr "C a + b"
        `shouldBe` Right (infixE "+" (ExprNonconstantVariant (Path Nothing "C") a) b)
    it "lets parentheses override precedence" $
      expr "(a + b) * c" `shouldBe` Right (infixE "*" (infixE "+" a b) c)

  describe "prefix operators" $ do
    let a = var "a"
        b = var "b"
    it "parses prefix symbols" $
      expr "!a" `shouldBe` Right (ExprPrefix "!" a)
    it "parses negation" $ do
      expr "-a" `shouldBe` Right (ExprPrefix "-" a)
      expr "-.a" `shouldBe` Right (ExprPrefix "-." a)
    it "binds negation tighter than binary operators" $
      expr "-a + b" `shouldBe` Right (infixE "+" (ExprPrefix "-" a) b)
    it "tells binary and unary minus apart" $
      expr "a - -b" `shouldBe` Right (infixE "-" a (ExprPrefix "-" b))
    it "applies negation to a whole function application" $
      expr "-f a"
        `shouldBe` Right (ExprPrefix "-" (ExprFunctionApplication (var "f") [UnlabeledArgument a]))
    it "binds prefix symbols tighter than negation" $
      expr "- !a" `shouldBe` Right (ExprPrefix "-" (ExprPrefix "!" a))
    it "binds prefix symbols tighter than application" $
      expr "!f x"
        `shouldBe` Right (ExprFunctionApplication (ExprPrefix "!" (var "f")) [UnlabeledArgument (var "x")])
    it "allows prefix applications as arguments" $
      expr "f !a b"
        `shouldBe` Right
          (ExprFunctionApplication (var "f") [UnlabeledArgument (ExprPrefix "!" a), UnlabeledArgument b])
    it "applies a prefix symbol to a parenthesized expression" $
      expr "!(f x)"
        `shouldBe` Right (ExprPrefix "!" (ExprFunctionApplication (var "f") [UnlabeledArgument (var "x")]))
    it "mixes with other operators" $
      expr "!a + !b" `shouldBe` Right (infixE "+" (ExprPrefix "!" a) (ExprPrefix "!" b))

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
