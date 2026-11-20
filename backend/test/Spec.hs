module Main (main) where

import Backend.Jasmin (generateJasmin)
import Data.List (isInfixOf)
import Frontend.Ast (Constant (..), Path (..))
import Frontend.Lexer (alexScanTokens)
import Frontend.Parser (parse)
import Frontend.Tokens (IntLiteral (IntLiteral), IntRepresentation (Unspecified))
import Intermediate.Type (ArgLabel (..), Type (..))
import Intermediate.TypeAnnotator (lowerAndInfer)
import Intermediate.TypedAst
import Test.Hspec

main :: IO ()
main = hspec $ describe "generateJasmin" $ do
  it "accepts the output of the lexer, parser, and type annotator" $ do
    let generated =
          alexScanTokens "let x = 40;; let y = x + 2"
            >>= parse
            >>= lowerAndInfer
            >>= generateJasmin "example/Program"
    withCode generated $ \code -> do
      code `shouldContain` ".class public example/Program\n"
      code `shouldContain` "  .limit locals 5\n"
      code `shouldContain` "  lload 1\n  ldc2_w 2\n  ladd\n"

  it "counts two-slot operands and shares the stack height at a conditional join" $ do
    let condition = TEConstant TBool (TConstBool False)
        choose = TEIf TFloat condition (double 1.5) (double 2.5)
        expression = binary "+." TFloat (double 3) choose
    withCode (generateExpressions [expression]) $ \code -> do
      code `shouldContain` "  .limit stack 4\n"
      code `shouldContain` "  ifeq L0\n  ldc2_w 1.5\n  goto L1\nL0:\n  ldc2_w 2.5\nL1:\n  dadd\n  pop2\n"

  it "evaluates parallel right-hand sides before bringing the new names into scope" $ do
    let variable name = TEVar TInt (Path Nothing name)
        bind name rhs = TLetBinding (TPVariable TInt name) rhs
        local = TELet TInt False [bind "x" (integer 2), bind "y" (variable "x")] (variable "y")
        program = TProgram [TDefinitionLet False [bind "x" (integer 1)], TDefinitionExpr local, TDefinitionExpr (variable "x")]
    withCode (generateJasmin "Program" program) $ \code ->
      code `shouldContain` "  lstore 3\n  lload 1\n  lstore 5\n  lload 5\n  pop2\n  lload 1\n"

  it "quotes control characters, quotes, backslashes, and non-BMP characters" $ do
    let expression = TEConstant TString (TConstString "a\n\"\\\x1f642")
    withCode (generateExpressions [expression]) $ \code ->
      code `shouldContain` "  ldc \"a\\u000a\\\"\\\\\\ud83d\\ude42\"\n"

  it "returns a diagnostic for an unsupported expression" $ do
    let expression = TEFunction (TArrow NoLabel TInt TInt) NoLabel Nothing []
    generateExpressions [expression] `shouldSatisfy` either (isInfixOf "function values") (const False)

  it "rejects invalid class names before rendering" $
    generateJasmin "example//Program" (TProgram []) `shouldBe` Left "Invalid JVM class name \"example//Program\""

  it "rejects unqualified names that could be Jasmin keywords" $
    generateJasmin "return" (TProgram []) `shouldBe` Left "Invalid JVM class name \"return\""

  it "rejects methods that exceed the JVM code size limit" $ do
    let program = TProgram (replicate 17000 (TDefinitionExpr (integer 1)))
    generateJasmin "Program" program `shouldSatisfy` either (isInfixOf "65,535-byte code limit") (const False)

  it "rejects branch targets outside a signed 16-bit offset" $ do
    let skipped = TLetBinding (TPWildcard TInt) (integer 1)
        largeBranch = TELet TUnit False (replicate 9000 skipped) (TEConstant TUnit TConstUnit)
        expression = TEIf TUnit (TEConstant TBool (TConstBool False)) largeBranch (TEConstant TUnit TConstUnit)
    generateExpressions [expression] `shouldSatisfy` either (isInfixOf "16-bit offset limit") (const False)

  -- lists are just another variant. We no longer need special treatment for them
  -- TODO: support variants
  -- it "allows a polymorphic empty list binding to be instantiated at another type" $ do
  --   let generated =
  --         alexScanTokens "let empty = [];; let copy = empty"
  --           >>= parse
  --           >>= lowerAndInfer
  --           >>= generateJasmin "Program"
  --   withCode generated $ \code ->
  --     code `shouldContain` "  aconst_null\n  astore 1\n  aload 1\n"

  it "allows a polymorphic empty array binding to be instantiated at another type" $ do
    let generated =
          alexScanTokens "let empty = [||];; let copy = empty"
            >>= parse
            >>= lowerAndInfer
            >>= generateJasmin "Program"
    withCode generated $ \code ->
      code `shouldContain` "  anewarray java/lang/Object\n  astore 1\n  aload 1\n"

generateExpressions :: [TExpr Type] -> Either String String
generateExpressions = generateJasmin "Program" . TProgram . map TDefinitionExpr

withCode :: Either String String -> (String -> Expectation) -> Expectation
withCode result check = either expectationFailure check result

integer :: Int -> TExpr Type
integer n = TEConstant TInt (TConstInt (IntLiteral n Unspecified))

double :: Double -> TExpr Type
double = TEConstant TFloat . TConstDouble

binary :: String -> Type -> TExpr Type -> TExpr Type -> TExpr Type
binary name ty left right =
  let inner = TArrow NoLabel ty ty
      operator = TEVar (TArrow NoLabel ty inner) (Path Nothing name)
   in TEApply ty (TEApply inner operator left) right
