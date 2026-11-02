module ParserSpec (spec) where

import Ast
import Lexer (alexScanTokens)
import MyParser (parse)
import Test.Hspec
import Tokens

-- lex and parse a whole source string
parseSource :: String -> Either String Program
parseSource src = alexScanTokens src >>= parse

-- the expressions of a program made only of items from the implementation
exprsOf :: String -> Either String [Expr]
exprsOf src = fmap items (parseSource src)
  where
    items (Program _ (UnitImplementation Nothing)) = []
    items (Program _ (UnitImplementation (Just (ModuleItems is)))) = [e | Expr e <- is]

-- parse a single expression
parseExpr :: String -> Either String Expr
parseExpr src = exprsOf src >>= single
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
infixE o l r = ExprInfix {op = o, left = l, right = r}

int :: Int -> Expr
int v = ExprConstant (ConstantInt (IntLiteral {Tokens.value = v, representation = Unspecified}))

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
      parseExpr "x" `shouldBe` Right (var "x")
    it "parses module-qualified values" $
      parseExpr "A.B.x" `shouldBe` Right (qualifiedVar ["A", "B"] "x")
    it "parses operators as values" $ do
      parseExpr "(+)" `shouldBe` Right (var "+")
      parseExpr "(mod)" `shouldBe` Right (var "mod")
      parseExpr "A.(+)" `shouldBe` Right (qualifiedVar ["A"] "+")
    it "parses unqualified constructors" $
      parseExpr "C" `shouldBe` Right (variant "C")
    it "parses qualified constructors" $
      parseExpr "A.B.C"
        `shouldBe` Right (ExprConstant (ConstantVariant (Path (Just (ModulePath ["A", "B"])) "C")))

  describe "constants" $ do
    it "parses literals" $ do
      parseExpr "1" `shouldBe` Right (int 1)
      parseExpr "1.5" `shouldBe` Right (ExprConstant (ConstantDouble 1.5))
      parseExpr "'a'" `shouldBe` Right (ExprConstant (ConstantChar 'a'))
      parseExpr "\"hi\"" `shouldBe` Right (ExprConstant (ConstantString "hi"))
    it "parses booleans" $ do
      parseExpr "true" `shouldBe` Right (ExprConstant (ConstantBool True))
      parseExpr "false" `shouldBe` Right (ExprConstant (ConstantBool False))
    it "parses unit both ways" $ do
      parseExpr "()" `shouldBe` Right (ExprConstant ConstantUnit)
      parseExpr "begin end" `shouldBe` Right (ExprConstant ConstantUnit)
    it "parses empty lists and arrays" $ do
      parseExpr "[]" `shouldBe` Right (ExprConstant ConstantList)
      parseExpr "[||]" `shouldBe` Right (ExprConstant ConstantArray)
    it "parses polymorphic variant tags" $
      parseExpr "`Tag" `shouldBe` Right (ExprConstant (ConstantPolymorphicVariant "Tag"))

  describe "parentheses" $ do
    it "parses parenthesized expressions" $
      parseExpr "(x)" `shouldBe` Right (var "x")
    it "parses begin ... end" $
      parseExpr "begin x end" `shouldBe` Right (var "x")

  describe "constructor application" $ do
    it "applies a constructor to an argument" $
      parseExpr "C 1" `shouldBe` Right (ExprNonconstantVariant {constr = Path Nothing "C", expr = int 1})
    it "applies a constructor to a parenthesized constructor application" $
      parseExpr "C (D 1)"
        `shouldBe` Right
          ( ExprNonconstantVariant
              { constr = Path Nothing "C",
                expr = ExprNonconstantVariant {constr = Path Nothing "D", expr = int 1}
              }
          )
    it "applies a constructor to a nullary constructor" $
      parseExpr "C D"
        `shouldBe` Right
          ( ExprNonconstantVariant
              { constr = Path Nothing "C",
                expr = variant "D"
              }
          )
    it "only takes a simple expression as its argument" $
      parseExpr "C f x" `shouldSatisfy` isLeft
    it "applies a polymorphic variant tag" $
      parseExpr "`Tag 1" `shouldBe` Right (ExprNonconstantPolymorphicVariant {tagname = "Tag", expr = int 1})

  describe "function application" $ do
    it "applies a function to one argument" $
      parseExpr "f 1"
        `shouldBe` Right
          ( ExprFunctionApplication
              { func = var "f",
                args = [UnlabeledArgument (int 1)]
              }
          )
    it "applies a function to several arguments at once" $
      parseExpr "f a b"
        `shouldBe` Right
          ( ExprFunctionApplication
              { func = var "f",
                args = [UnlabeledArgument (var "a"), UnlabeledArgument (var "b")]
              }
          )
    it "treats a parenthesized application as a single argument" $
      parseExpr "f (g a) b"
        `shouldBe` Right
          ( ExprFunctionApplication
              { func = var "f",
                args =
                  [ UnlabeledArgument
                      ( ExprFunctionApplication
                          { func = var "g",
                            args = [UnlabeledArgument (var "a")]
                          }
                      ),
                    UnlabeledArgument (var "b")
                  ]
              }
          )
    it "allows nullary constructors as arguments" $
      parseExpr "f C 1"
        `shouldBe` Right
          ( ExprFunctionApplication
              { func = var "f",
                args = [UnlabeledArgument (variant "C"), UnlabeledArgument (int 1)]
              }
          )
    it "applies qualified functions" $
      parseExpr "List.map f"
        `shouldBe` Right
          ( ExprFunctionApplication
              { func = qualifiedVar ["List"] "map",
                args = [UnlabeledArgument (var "f")]
              }
          )
    it "parses labeled and optional arguments" $
      parseExpr "f ~x:1 ~y ?z:2 ?w"
        `shouldBe` Right
          ( ExprFunctionApplication
              { func = var "f",
                args =
                  [ LabeledArgument "x" (Just (int 1)),
                    LabeledArgument "y" Nothing,
                    OptionalArgument "z" (Just (int 2)),
                    OptionalArgument "w" Nothing
                  ]
              }
          )
    it "binds tighter than ," $
      parseExpr "f a, g b"
        `shouldBe` Right
          ( ExprTuple
              [ ExprFunctionApplication {func = var "f", args = [UnlabeledArgument (var "a")]},
                ExprFunctionApplication {func = var "g", args = [UnlabeledArgument (var "b")]}
              ]
          )
    it "binds tighter than ::" $
      parseExpr "f a :: b"
        `shouldBe` Right
          ( ExprCons
              [ ExprFunctionApplication
                  { func = var "f",
                    args = [UnlabeledArgument (var "a")]
                  },
                var "b"
              ]
          )
    it "can't be the application of a bare constructor" $
      parseExpr "C 1" `shouldBe` Right (ExprNonconstantVariant {constr = Path Nothing "C", expr = int 1})

  describe "infix operators" $ do
    let a = var "a"
        b = var "b"
        c = var "c"
    it "parses a single operator from each class" $ do
      parseExpr "a <= b" `shouldBe` Right (infixE "<=" a b)
      parseExpr "a @ b" `shouldBe` Right (infixE "@" a b)
      parseExpr "a +. b" `shouldBe` Right (infixE "+." a b)
      parseExpr "a /. b" `shouldBe` Right (infixE "/." a b)
      parseExpr "a ** b" `shouldBe` Right (infixE "**" a b)
      parseExpr "a #+ b" `shouldBe` Right (infixE "#+" a b)
    it "parses operators that have their own tokens" $ do
      parseExpr "a = b" `shouldBe` Right (infixE "=" a b)
      parseExpr "a != b" `shouldBe` Right (infixE "!=" a b)
      parseExpr "a && b" `shouldBe` Right (infixE "&&" a b)
      parseExpr "a || b" `shouldBe` Right (infixE "||" a b)
      parseExpr "a := b" `shouldBe` Right (infixE ":=" a b)
    it "parses keyword operators" $ do
      parseExpr "a mod b" `shouldBe` Right (infixE "mod" a b)
      parseExpr "a lsl b" `shouldBe` Right (infixE "lsl" a b)
      parseExpr "a or b" `shouldBe` Right (infixE "or" a b)
    it "makes * bind tighter than +" $
      parseExpr "a + b * c" `shouldBe` Right (infixE "+" a (infixE "*" b c))
    it "makes + bind looser than a custom * operator" $
      parseExpr "a *. b +. c" `shouldBe` Right (infixE "+." (infixE "*." a b) c)
    it "treats operators of the same class as one level" $
      parseExpr "a + b -. c" `shouldBe` Right (infixE "-." (infixE "+" a b) c)
    it "treats keyword operators as part of their class" $
      parseExpr "a * b mod c" `shouldBe` Right (infixE "mod" (infixE "*" a b) c)
    it "associates + to the left" $
      parseExpr "a + b + c" `shouldBe` Right (infixE "+" (infixE "+" a b) c)
    it "associates ** to the right" $
      parseExpr "a ** b ** c" `shouldBe` Right (infixE "**" a (infixE "**" b c))
    it "associates @ to the right" $
      parseExpr "a @ b @ c" `shouldBe` Right (infixE "@" a (infixE "@" b c))
    it "associates = to the left" $
      parseExpr "a = b = c" `shouldBe` Right (infixE "=" (infixE "=" a b) c)
    it "associates && to the right" $
      parseExpr "a && b && c" `shouldBe` Right (infixE "&&" a (infixE "&&" b c))
    it "associates := to the right" $
      parseExpr "a := b := c" `shouldBe` Right (infixE ":=" a (infixE ":=" b c))
    it "binds + tighter than ::" $
      parseExpr "a + b :: c" `shouldBe` Right (ExprCons [infixE "+" a b, c])
    it "binds :: tighter than @" $
      parseExpr "a :: b @ c" `shouldBe` Right (infixE "@" (ExprCons [a, b]) c)
    it "binds @ tighter than =" $
      parseExpr "a @ b = c" `shouldBe` Right (infixE "=" (infixE "@" a b) c)
    it "binds = tighter than &&" $
      parseExpr "a = b && c" `shouldBe` Right (infixE "&&" (infixE "=" a b) c)
    it "binds && tighter than ||" $
      parseExpr "a || b && c" `shouldBe` Right (infixE "||" a (infixE "&&" b c))
    it "binds || tighter than ," $
      parseExpr "a || b, c" `shouldBe` Right (ExprTuple [infixE "||" a b, c])
    it "binds , tighter than :=" $
      parseExpr "a := b, c" `shouldBe` Right (infixE ":=" a (ExprTuple [b, c]))
    it "binds function application tighter than operators" $
      parseExpr "f a + g b"
        `shouldBe` Right
          ( infixE
              "+"
              (ExprFunctionApplication {func = var "f", args = [UnlabeledArgument a]})
              (ExprFunctionApplication {func = var "g", args = [UnlabeledArgument b]})
          )
    it "binds constructor application tighter than operators" $
      parseExpr "C a + b"
        `shouldBe` Right (infixE "+" (ExprNonconstantVariant {constr = Path Nothing "C", expr = a}) b)
    it "lets parentheses override precedence" $
      parseExpr "(a + b) * c" `shouldBe` Right (infixE "*" (infixE "+" a b) c)

  describe "prefix operators" $ do
    let a = var "a"
        b = var "b"
    it "parses prefix symbols" $
      parseExpr "!a" `shouldBe` Right (ExprPrefix {op = "!", expr = a})
    it "parses negation" $ do
      parseExpr "-a" `shouldBe` Right (ExprPrefix {op = "-", expr = a})
      parseExpr "-.a" `shouldBe` Right (ExprPrefix {op = "-.", expr = a})
    it "binds negation tighter than binary operators" $
      parseExpr "-a + b" `shouldBe` Right (infixE "+" (ExprPrefix {op = "-", expr = a}) b)
    it "tells binary and unary minus apart" $
      parseExpr "a - -b" `shouldBe` Right (infixE "-" a (ExprPrefix {op = "-", expr = b}))
    it "applies negation to a whole function application" $
      parseExpr "-f a"
        `shouldBe` Right
          ( ExprPrefix
              { op = "-",
                expr = ExprFunctionApplication {func = var "f", args = [UnlabeledArgument a]}
              }
          )
    it "binds prefix symbols tighter than negation" $
      parseExpr "- !a" `shouldBe` Right (ExprPrefix {op = "-", expr = ExprPrefix {op = "!", expr = a}})
    it "binds prefix symbols tighter than application" $
      parseExpr "!f x"
        `shouldBe` Right
          ( ExprFunctionApplication
              { func = ExprPrefix {op = "!", expr = var "f"},
                args = [UnlabeledArgument (var "x")]
              }
          )
    it "allows prefix applications as arguments" $
      parseExpr "f !a b"
        `shouldBe` Right
          ( ExprFunctionApplication
              { func = var "f",
                args = [UnlabeledArgument (ExprPrefix {op = "!", expr = a}), UnlabeledArgument b]
              }
          )
    it "applies a prefix symbol to a parenthesized expression" $
      parseExpr "!(f x)"
        `shouldBe` Right
          ( ExprPrefix
              { op = "!",
                expr = ExprFunctionApplication {func = var "f", args = [UnlabeledArgument (var "x")]}
              }
          )
    it "mixes with other operators" $
      parseExpr "!a + !b"
        `shouldBe` Right
          ( infixE
              "+"
              ( ExprPrefix
                  { op = "!",
                    expr = a
                  }
              )
              (ExprPrefix {op = "!", expr = b})
          )

  describe "field access and assignment" $ do
    let a = var "a"
        b = var "b"
        c = var "c"
        fieldPath name = Path Nothing name
    it "parses field access" $
      parseExpr "a.f" `shouldBe` Right (ExprFieldAccess {expr = a, field = fieldPath "f"})
    it "parses field assignment" $
      parseExpr "a.f <- b"
        `shouldBe` Right
          ( ExprFieldAssignment
              { expr = a,
                field = fieldPath "f",
                Ast.value = b
              }
          )
    it "reads a module path before the dot as a path, not a constructor" $
      parseExpr "!M.x" `shouldBe` Right (ExprPrefix {op = "!", expr = qualifiedVar ["M"] "x"})
    it "binds a prefix symbol tighter than field access" $
      parseExpr "!a.f"
        `shouldBe` Right
          ( ExprFieldAccess
              { expr = ExprPrefix {op = "!", expr = a},
                field = fieldPath "f"
              }
          )
    it "lets the assigned value be an operator expression" $
      parseExpr "a.f <- b + c"
        `shouldBe` Right
          ( ExprFieldAssignment
              { expr = a,
                field = fieldPath "f",
                Ast.value = infixE "+" b c
              }
          )
    it "lets the assigned value be a tuple" $
      parseExpr "a.f <- b, c"
        `shouldBe` Right
          ( ExprFieldAssignment
              { expr = a,
                field = fieldPath "f",
                Ast.value = ExprTuple [b, c]
              }
          )
    it "associates <- and := together to the right" $
      parseExpr "a.f <- b := c"
        `shouldBe` Right
          ( ExprFieldAssignment
              { expr = a,
                field = fieldPath "f",
                Ast.value = infixE ":=" b c
              }
          )

  describe "tuples" $ do
    it "parses a flat tuple" $
      parseExpr "a, b, c" `shouldBe` Right (ExprTuple [var "a", var "b", var "c"])
    it "keeps a parenthesized tuple on the left nested" $
      parseExpr "(a, b), c" `shouldBe` Right (ExprTuple [ExprTuple [var "a", var "b"], var "c"])
    it "keeps a parenthesized tuple on the right nested" $
      parseExpr "a, (b, c)" `shouldBe` Right (ExprTuple [var "a", ExprTuple [var "b", var "c"]])
    it "binds constructor application tighter than ," $
      parseExpr "C a, b"
        `shouldBe` Right
          ( ExprTuple
              [ ExprNonconstantVariant
                  { constr = Path Nothing "C",
                    expr = var "a"
                  },
                var "b"
              ]
          )
    it "lets parentheses move the comma inside the application" $
      parseExpr "C (a, b)"
        `shouldBe` Right
          ( ExprNonconstantVariant
              { constr = Path Nothing "C",
                expr = ExprTuple [var "a", var "b"]
              }
          )

  describe "cons" $ do
    it "parses a chain of ::" $
      parseExpr "a :: b :: c" `shouldBe` Right (ExprCons [var "a", var "b", var "c"])
    it "binds :: tighter than ," $
      parseExpr "a :: b, c"
        `shouldBe` Right (ExprTuple [ExprCons [var "a", var "b"], var "c"])
    it "binds constructor application tighter than ::" $
      parseExpr "C a :: b"
        `shouldBe` Right
          ( ExprCons
              [ ExprNonconstantVariant
                  { constr = Path Nothing "C",
                    expr = var "a"
                  },
                var "b"
              ]
          )

  describe "lists and arrays" $ do
    it "parses list literals as a cons chain ending in []" $
      parseExpr "[a; b]" `shouldBe` Right (ExprCons [var "a", var "b", ExprConstant ConstantList])
    it "allows a trailing ; in list literals" $
      parseExpr "[a; b;]" `shouldBe` parseExpr "[a; b]"
    it "parses array literals" $
      parseExpr "[|a; b|]" `shouldBe` Right (ExprArray [var "a", var "b"])

  describe "sequences" $ do
    let a = var "a"
        b = var "b"
        c = var "c"
        seqE l r = ExprSequence {left = l, right = r}
        fieldPath = Path Nothing
    it "parses a sequence at the top level" $
      parseExpr "a; b" `shouldBe` Right (seqE a b)
    it "associates ; to the right" $
      parseExpr "a; b; c" `shouldBe` Right (seqE a (seqE b c))
    it "parses sequences inside parentheses and begin ... end" $ do
      parseExpr "(a; b)" `shouldBe` Right (seqE a b)
      parseExpr "begin a; b end" `shouldBe` Right (seqE a b)
    it "binds every other operator tighter than ;" $
      parseExpr "a := b; c, a" `shouldBe` Right (seqE (infixE ":=" a b) (ExprTuple [c, a]))
    it "treats ; in a list as an item separator" $
      parseExpr "[a; b; c]"
        `shouldBe` Right (ExprCons [a, b, c, ExprConstant ConstantList])
    it "treats ; in an array as an item separator" $
      parseExpr "[|a; b; c|]" `shouldBe` Right (ExprArray [a, b, c])
    it "allows a parenthesized sequence as a list item" $
      parseExpr "[(a; b); c]"
        `shouldBe` Right (ExprCons [seqE a b, c, ExprConstant ConstantList])
    it "treats ; in a record as a field separator" $
      parseExpr "{x = a; y = b}"
        `shouldBe` Right
          ( ExprRecord
              { source = Nothing,
                fields =
                  [ RecordField (fieldPath "x") Nothing a,
                    RecordField (fieldPath "y") Nothing b
                  ]
              }
          )
    it "ends an if branch at ;" $
      parseExpr "if a then b; c"
        `shouldBe` Right (seqE (ExprIf {cond = a, thenExpr = b, elseExpr = Nothing}) c)
    it "allows sequences in a loop body" $
      parseExpr "while a do b; c done"
        `shouldBe` Right (ExprWhile {cond = a, body = seqE b c})

  describe "type annotations"
    $ it "has no type expressions yet"
    $ parseExpr "(x : t)" `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
