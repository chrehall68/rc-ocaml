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

intPat :: Int -> Pattern
intPat v = PatternConstant (ConstantInt (IntLiteral {Tokens.value = v, representation = Unspecified}))

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

  describe "match" $ do
    let a = var "a"
        b = var "b"
        x = var "x"
        y = var "y"
        matchE e cs = ExprMatch {expr = e, cases = cs}
        seqE l r = ExprSequence {left = l, right = r}
    it "parses a single case" $
      parseExpr "match x with 1 -> a" `shouldBe` Right (matchE x [(intPat 1, Nothing, a)])
    it "parses several cases, with or without a leading |" $ do
      let expected = Right (matchE x [(intPat 1, Nothing, a), (intPat 2, Nothing, b)])
      parseExpr "match x with 1 -> a | 2 -> b" `shouldBe` expected
      parseExpr "match x with | 1 -> a | 2 -> b" `shouldBe` expected
    it "parses guards" $
      parseExpr "match x with 1 when y -> a | 2 -> b"
        `shouldBe` Right (matchE x [(intPat 1, Just y, a), (intPat 2, Nothing, b)])
    it "allows a sequence as the scrutinee" $
      parseExpr "match a; b with 1 -> x" `shouldBe` Right (matchE (seqE a b) [(intPat 1, Nothing, x)])
    it "extends the last case as far as possible" $ do
      parseExpr "match x with 1 -> a + b"
        `shouldBe` Right (matchE x [(intPat 1, Nothing, infixE "+" a b)])
      parseExpr "match x with 1 -> a, b"
        `shouldBe` Right (matchE x [(intPat 1, Nothing, ExprTuple [a, b])])
      parseExpr "match x with 1 -> a; b"
        `shouldBe` Right (matchE x [(intPat 1, Nothing, seqE a b)])
    it "lets parentheses end a match" $
      parseExpr "(match x with 1 -> a) + b"
        `shouldBe` Right (infixE "+" (matchE x [(intPat 1, Nothing, a)]) b)
    it "gives later cases to the innermost match" $
      parseExpr "match x with 1 -> match y with 2 -> a | 3 -> b"
        `shouldBe` Right
          (matchE x [(intPat 1, Nothing, matchE y [(intPat 2, Nothing, a), (intPat 3, Nothing, b)])])
    it "lets parentheses give later cases to the outer match" $
      parseExpr "match x with 1 -> (match y with 2 -> a) | 3 -> b"
        `shouldBe` Right
          (matchE x [(intPat 1, Nothing, matchE y [(intPat 2, Nothing, a)]), (intPat 3, Nothing, b)])

  describe "function" $ do
    let a = var "a"
        b = var "b"
        x = var "x"
    it "parses a single case" $
      parseExpr "function 1 -> a" `shouldBe` Right (ExprFunction {cases = [(intPat 1, Nothing, a)]})
    it "parses several cases, with or without a leading |" $ do
      let expected = Right (ExprFunction {cases = [(intPat 1, Nothing, a), (intPat 2, Nothing, b)]})
      parseExpr "function 1 -> a | 2 -> b" `shouldBe` expected
      parseExpr "function | 1 -> a | 2 -> b" `shouldBe` expected
    it "parses guards" $
      parseExpr "function 1 when x -> a"
        `shouldBe` Right (ExprFunction {cases = [(intPat 1, Just x, a)]})
    it "gives later cases to an inner match" $
      parseExpr "function 1 -> match x with 2 -> a | 3 -> b"
        `shouldBe` Right
          ( ExprFunction
              { cases =
                  [ ( intPat 1,
                      Nothing,
                      ExprMatch {expr = x, cases = [(intPat 2, Nothing, a), (intPat 3, Nothing, b)]}
                    )
                  ]
              }
          )

  describe "fun" $ do
    let a = var "a"
        b = var "b"
        lambda ps e = ExprLambda {parameters = ps, typeConstraint = Nothing, body = e}
    it "parses a single pattern parameter" $
      parseExpr "fun 1 -> a" `shouldBe` Right (lambda [ParameterPattern (intPat 1)] a)
    it "parses several parameters" $
      parseExpr "fun 1 2 -> a"
        `shouldBe` Right (lambda [ParameterPattern (intPat 1), ParameterPattern (intPat 2)] a)
    it "parses a return type constraint" $
      parseExpr "fun 1 : _ -> a"
        `shouldBe` Right
          (ExprLambda {parameters = [ParameterPattern (intPat 1)], typeConstraint = Just TypeExpr, body = a})
    it "extends the body as far as possible" $
      parseExpr "fun 1 -> a; b"
        `shouldBe` Right (lambda [ParameterPattern (intPat 1)] (ExprSequence {left = a, right = b}))
    it "lets parentheses end the body" $
      parseExpr "(fun 1 -> a) b"
        `shouldBe` Right
          ( ExprFunctionApplication
              { func = lambda [ParameterPattern (intPat 1)] a,
                args = [UnlabeledArgument b]
              }
          )
    it "parses labeled parameters" $ do
      parseExpr "fun ~x -> a" `shouldBe` Right (lambda [ParameterLabel "x" Nothing Nothing] a)
      parseExpr "fun ~(x) -> a" `shouldBe` Right (lambda [ParameterLabel "x" Nothing Nothing] a)
      parseExpr "fun ~(x : _) -> a" `shouldBe` Right (lambda [ParameterLabel "x" (Just TypeExpr) Nothing] a)
      parseExpr "fun ~x:1 -> a" `shouldBe` Right (lambda [ParameterLabel "x" Nothing (Just (intPat 1))] a)
    it "parses optional parameters" $ do
      parseExpr "fun ?x -> a" `shouldBe` Right (lambda [ParameterOptional "x" Nothing Nothing] a)
      parseExpr "fun ?(x) -> a" `shouldBe` Right (lambda [ParameterOptional "x" Nothing Nothing] a)
      parseExpr "fun ?(x : _) -> a" `shouldBe` Right (lambda [ParameterOptional "x" (Just TypeExpr) Nothing] a)
      parseExpr "fun ?(x = b) -> a" `shouldBe` Right (lambda [ParameterOptional "x" Nothing (Just b)] a)
      parseExpr "fun ?(x : _ = b) -> a"
        `shouldBe` Right (lambda [ParameterOptional "x" (Just TypeExpr) (Just b)] a)
    it "parses optional parameters with patterns" $ do
      parseExpr "fun ?x:1 -> a"
        `shouldBe` Right (lambda [ParameterOptionalPattern "x" (intPat 1) Nothing Nothing] a)
      parseExpr "fun ?x:(1) -> a"
        `shouldBe` Right (lambda [ParameterOptionalPattern "x" (intPat 1) Nothing Nothing] a)
      parseExpr "fun ?x:(1 : _ = b) -> a"
        `shouldBe` Right (lambda [ParameterOptionalPattern "x" (intPat 1) (Just TypeExpr) (Just b)] a)
    -- each expected tree below matches what `ocamlc -stop-after parsing -dsource` prints
    it "takes a whole sequence as an optional parameter's default" $ do
      let c = var "c"
          d = var "d"
          seqE l r = ExprSequence {left = l, right = r}
      parseExpr "fun ?(x = b; c) -> a"
        `shouldBe` Right (lambda [ParameterOptional "x" Nothing (Just (seqE b c))] a)
      parseExpr "fun ?(x : _ = b; c) -> a"
        `shouldBe` Right (lambda [ParameterOptional "x" (Just TypeExpr) (Just (seqE b c))] a)
      parseExpr "fun ?x:(1 = b; c) -> a"
        `shouldBe` Right (lambda [ParameterOptionalPattern "x" (intPat 1) Nothing (Just (seqE b c))] a)
      parseExpr "fun ?x:(1 : _ = b; c) -> a"
        `shouldBe` Right (lambda [ParameterOptionalPattern "x" (intPat 1) (Just TypeExpr) (Just (seqE b c))] a)
      parseExpr "fun ?(x = b; c; d) -> a"
        `shouldBe` Right (lambda [ParameterOptional "x" Nothing (Just (seqE b (seqE c d)))] a)
    it "takes any expression as an optional parameter's default" $ do
      let c = var "c"
      parseExpr "fun ?(x = b, c) -> a"
        `shouldBe` Right (lambda [ParameterOptional "x" Nothing (Just (ExprTuple [b, c]))] a)
      parseExpr "fun ?(x = b := c) -> a"
        `shouldBe` Right (lambda [ParameterOptional "x" Nothing (Just (infixE ":=" b c))] a)
      parseExpr "fun ?(x = if b then c) -> a"
        `shouldBe` Right
          (lambda [ParameterOptional "x" Nothing (Just (ExprIf {cond = b, thenExpr = c, elseExpr = Nothing}))] a)
      parseExpr "fun ?(x = match b with 1 -> c | 2 -> a) -> a"
        `shouldBe` Right
          ( lambda
              [ ParameterOptional
                  "x"
                  Nothing
                  (Just (ExprMatch {expr = b, cases = [(intPat 1, Nothing, c), (intPat 2, Nothing, a)]}))
              ]
              a
          )
    it "keeps the default inside the parentheses" $
      parseExpr "fun ?(x = b) 1 -> a"
        `shouldBe` Right (lambda [ParameterOptional "x" Nothing (Just b), ParameterPattern (intPat 1)] a)
    it "mixes parameter kinds" $
      parseExpr "fun 1 ~x ?y -> a"
        `shouldBe` Right
          ( lambda
              [ParameterPattern (intPat 1), ParameterLabel "x" Nothing Nothing, ParameterOptional "y" Nothing Nothing]
              a
          )
    it "needs at least one parameter" $
      parseExpr "fun -> a" `shouldSatisfy` isLeft

  describe "try" $ do
    let a = var "a"
        b = var "b"
        c = var "c"
    it "parses a single handler" $
      parseExpr "try a with 1 -> b" `shouldBe` Right (ExprTry {expr = a, cases = [(intPat 1, Nothing, b)]})
    it "parses several handlers, with or without a leading |" $ do
      let expected = Right (ExprTry {expr = a, cases = [(intPat 1, Nothing, b), (intPat 2, Nothing, c)]})
      parseExpr "try a with 1 -> b | 2 -> c" `shouldBe` expected
      parseExpr "try a with | 1 -> b | 2 -> c" `shouldBe` expected
    it "allows a sequence as the body" $
      parseExpr "try a; b with 1 -> c"
        `shouldBe` Right (ExprTry {expr = ExprSequence {left = a, right = b}, cases = [(intPat 1, Nothing, c)]})
    it "parses guards" $
      parseExpr "try a with 1 when b -> c" `shouldBe` Right (ExprTry {expr = a, cases = [(intPat 1, Just b, c)]})

  -- each expected tree below matches what `ocamlc -stop-after parsing -dsource` prints
  describe "trailing ;" $ do
    let a = var "a"
        b = var "b"
        c = var "c"
        x = var "x"
        seqE l r = ExprSequence {left = l, right = r}
    it "drops a trailing ; after a single expression" $ do
      parseExpr "(a;)" `shouldBe` Right a
      parseExpr "begin a; end" `shouldBe` Right a
    it "drops a trailing ; after a sequence" $ do
      parseExpr "(a; b;)" `shouldBe` Right (seqE a b)
      parseExpr "begin a; b; end" `shouldBe` Right (seqE a b)
    it "allows a trailing ; at the top level" $
      exprsOf "a; ;; b;" `shouldBe` Right [a, b]
    it "allows a trailing ; before a type annotation" $ do
      parseExpr "(a; : _)" `shouldBe` Right (ExprAnnotated {expr = a, typeExpr = TypeExpr})
      parseExpr "(a; b : _)" `shouldBe` Right (ExprAnnotated {expr = seqE a b, typeExpr = TypeExpr})
    it "allows a trailing ; in conditions and loops" $ do
      parseExpr "if a; then b" `shouldBe` Right (ExprIf {cond = a, thenExpr = b, elseExpr = Nothing})
      parseExpr "while a; do b; done" `shouldBe` Right (ExprWhile {cond = a, body = b})
      parseExpr "for i = a; to b; do c; done"
        `shouldBe` Right (ExprFor {valueName = "i", start = a, end = b, body = c, isTo = True})
    it "allows a trailing ; in array and string indices" $ do
      parseExpr "x.(a;)" `shouldBe` Right (ExprArrayAccess {arr = x, index = a})
      parseExpr "x.[a;]" `shouldBe` Right (ExprStringAccess {str = x, index = a})
    it "allows a trailing ; in match, try and function cases" $ do
      parseExpr "match a; with 1 -> b" `shouldBe` Right (ExprMatch {expr = a, cases = [(intPat 1, Nothing, b)]})
      parseExpr "try a; with 1 -> b" `shouldBe` Right (ExprTry {expr = a, cases = [(intPat 1, Nothing, b)]})
      parseExpr "match x with 1 -> a; | 2 -> b"
        `shouldBe` Right (ExprMatch {expr = x, cases = [(intPat 1, Nothing, a), (intPat 2, Nothing, b)]})
      parseExpr "match x with 1 when a; -> b"
        `shouldBe` Right (ExprMatch {expr = x, cases = [(intPat 1, Just a, b)]})
      parseExpr "function 1 -> a; | 2 -> b"
        `shouldBe` Right (ExprFunction {cases = [(intPat 1, Nothing, a), (intPat 2, Nothing, b)]})
    it "allows a trailing ; in fun bodies and defaults" $ do
      let lambda ps e = ExprLambda {parameters = ps, typeConstraint = Nothing, body = e}
      parseExpr "(fun 1 -> a;)" `shouldBe` Right (lambda [ParameterPattern (intPat 1)] a)
      parseExpr "fun ?(x = a;) -> c" `shouldBe` Right (lambda [ParameterOptional "x" Nothing (Just a)] c)
    it "still treats a trailing ; in lists, arrays and records as a separator" $ do
      parseExpr "[a; b;]" `shouldBe` Right (ExprCons [a, b, ExprConstant ConstantList])
      parseExpr "[|a; b;|]" `shouldBe` Right (ExprArray [a, b])
      parseExpr "{x = a;}"
        `shouldBe` Right (ExprRecord {source = Nothing, fields = [RecordField (Path Nothing "x") Nothing a]})
    it "rejects more than one trailing ;" $ do
      parseExpr "(a; ;)" `shouldSatisfy` isLeft
      parseExpr "(a;;)" `shouldSatisfy` isLeft
    it "rejects a ; after a type annotation" $
      parseExpr "(a : _;)" `shouldSatisfy` isLeft

  describe "type annotations" $ do
    it "parses the _ placeholder type" $
      parseExpr "(x : _)" `shouldBe` Right (ExprAnnotated {expr = var "x", typeExpr = TypeExpr})
    it "has no other type expressions yet" $
      parseExpr "(x : t)" `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
