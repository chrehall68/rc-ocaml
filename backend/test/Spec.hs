module Main (main) where

import Backend.Jasmin (generateJasmin)
import Data.List (isInfixOf)
import Frontend.Ast (Constant (..), Path (..))
import Frontend.Tokens (IntLiteral (IntLiteral), IntRepresentation (Unspecified))
import Intermediate.Type (ArgLabel (..), Type (..))
import Intermediate.TypedAst
import Test.Hspec

main :: IO ()
main = hspec $ describe "generateJasmin" $ do
  it "counts two-slot operands and shares the stack height at a conditional join" $ do
    let condition = TEConstant TBool (ConstantBool False)
        choose = TEIf TDouble condition (double 1.5) (double 2.5)
        expression = binary "+." TDouble (double 3) choose
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

  it "returns a diagnostic for an unsupported expression" $ do
    let expression = TEFunction (TArrow NoLabel TInt TInt) NoLabel Nothing []
    generateExpressions [expression] `shouldSatisfy` either (isInfixOf "function values") (const False)

  it "rejects invalid class names before rendering" $
    generateJasmin "example//Program" (TProgram []) `shouldBe` Left "Invalid JVM class name \"example//Program\""

generateExpressions :: [TExpr Type] -> Either String String
generateExpressions = generateJasmin "Program" . TProgram . map TDefinitionExpr

withCode :: Either String String -> (String -> Expectation) -> Expectation
withCode result check = either expectationFailure check result

integer :: Int -> TExpr Type
integer n = TEConstant TInt (ConstantInt (IntLiteral n Unspecified))

double :: Double -> TExpr Type
double = TEConstant TDouble . ConstantDouble

binary :: String -> Type -> TExpr Type -> TExpr Type -> TExpr Type
binary name ty left right =
  let inner = TArrow NoLabel ty ty
      operator = TEVar (TArrow NoLabel ty inner) (Path Nothing name)
   in TEApply ty (TEApply inner operator NoLabel left) NoLabel right
