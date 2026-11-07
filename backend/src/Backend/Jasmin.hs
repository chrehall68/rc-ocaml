module Backend.Jasmin (generateJasmin) where

import Control.Monad (foldM, unless, when)
import Control.Monad.State.Strict (StateT, get, lift, put, runStateT)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
import qualified Data.Map.Strict as Map
import Frontend.Ast (Constant (..), Path (..))
import Frontend.Tokens (IntLiteral (IntLiteral), IntRepresentation (Unspecified))
import Intermediate.Type (Type (..))
import Intermediate.TypedAst

-- | Emit a Jasmin class with a static main method.
-- The class name uses JVM internal form, for example "example/Program".
-- Unsupported typed AST nodes return an error.
generateJasmin :: String -> TProgram Type -> Either String String
generateJasmin className (TProgram definitions) = do
  unless (validClassName className) $ Left ("Invalid JVM class name " ++ show className)
  ((_, code), finalState) <- runStateT (foldM emitDefinition (Map.empty, mempty) definitions) (Supply 1)
  let locals = nextLocal finalState
  when (locals > 65535 || peak code > 65535) $ Left "JVM method exceeds the stack or locals limit"
  pure $ unlines $
    [ ".class public " ++ className,
      ".super java/lang/Object",
      "",
      ".method public static main([Ljava/lang/String;)V",
      "  .limit stack " ++ show (peak code),
      "  .limit locals " ++ show locals
    ] ++ render code ["  return", ".end method"]

validClassName :: String -> Bool
validClassName name = case break (== '/') name of
  ([], _) -> False
  (first : rest, suffix) ->
    identifierStart first && all (\c -> identifierStart c || isDigit c) rest
      && case suffix of
        [] -> True
        _ : remaining -> validClassName remaining
  where
    identifierStart c = isAsciiLower c || isAsciiUpper c || c == '_' || c == '$'

-- Stack heights are measured in JVM slots. Double and long use two slots.
-- Each compiled expression leaves only its result above the incoming stack.
data Code = Code {delta :: Int, peak :: Int, render :: [String] -> [String]}

instance Semigroup Code where
  first <> second = Code
    (delta first + delta second)
    (max (peak first) (delta first + peak second))
    (render first . render second)

instance Monoid Code where
  mempty = Code 0 0 id

instruction :: Int -> String -> Code
instruction change name = Code change (max 0 change) (("  " ++ name) :)

data ValueKind = UnitValue | IntValue | LongValue | DoubleValue

kind :: Type -> Emit ValueKind
kind ty = case ty of
  TUnit -> pure UnitValue
  TInt -> pure LongValue
  TBool -> pure IntValue
  TChar -> pure IntValue
  TDouble -> pure DoubleValue
  _ -> reject ("JVM representation for " ++ show ty)

width :: ValueKind -> Int
width valueKind = case valueKind of
  UnitValue -> 0
  IntValue -> 1
  LongValue -> 2
  DoubleValue -> 2

data Local = Local Type ValueKind Int
type Env = Map.Map String Local

newtype Supply = Supply {nextLocal :: Int}
type Emit = StateT Supply (Either String)

reject :: String -> Emit a
reject feature = lift (Left ("Jasmin backend does not support " ++ feature))

requireType :: Type -> Type -> Emit ()
requireType expected actual = unless (expected == actual) $
  lift (Left ("Expected " ++ show expected ++ " but received " ++ show actual ++ " in lowered AST"))

freshLocal :: Type -> Emit Local
freshLocal ty = do
  valueKind <- kind ty
  supply <- get
  let slot = nextLocal supply
  put supply {nextLocal = slot + width valueKind}
  pure (Local ty valueKind slot)

discard :: ValueKind -> Code
discard valueKind = case width valueKind of
  0 -> mempty
  1 -> instruction (-1) "pop"
  _ -> instruction (-2) "pop2"

access :: String -> Int -> Local -> Code
access operation direction (Local _ valueKind slot) =
  let op prefix = instruction (direction * width valueKind) (prefix : operation ++ " " ++ show slot)
   in case valueKind of
        UnitValue -> mempty
        IntValue -> op 'i'
        LongValue -> op 'l'
        DoubleValue -> op 'd'

emitDefinition :: (Env, Code) -> TDefinition Type -> Emit (Env, Code)
emitDefinition (env, code) definition = case definition of
  TDefinitionType _ -> pure (env, code)
  TDefinitionLet recursive bindings -> do
    (newEnv, bindingCode) <- emitBindings env recursive bindings
    pure (newEnv, code <> bindingCode)
  TDefinitionExpr expression -> do
    expressionCode <- emitDiscarded env expression
    pure (env, code <> expressionCode)

emitBindings :: Env -> Bool -> [TLetBinding Type] -> Emit (Env, Code)
emitBindings _ True _ = reject "recursive bindings"
emitBindings env False [TLetBinding (TPVariable ty name) rhs] = do
  requireType ty (exprAnnotation rhs)
  rhsCode <- emitExpr env rhs
  local <- freshLocal ty
  pure (Map.insert name local env, rhsCode <> access "store" (-1) local)
emitBindings _ False _ = reject "binding groups and patterns other than a variable"

emitDiscarded :: Env -> TExpr Type -> Emit Code
emitDiscarded env expression = do
  code <- emitExpr env expression
  valueKind <- kind (exprAnnotation expression)
  pure (code <> discard valueKind)

emitExpr :: Env -> TExpr Type -> Emit Code
emitExpr env expression = case expression of
  TEConstant ty constant -> emitConstant ty constant
  TEVar ty (Path Nothing name) -> case Map.lookup name env of
    Just local@(Local boundType _ _) -> do
      requireType ty boundType
      pure (access "load" 1 local)
    Nothing -> reject ("unbound or first-class value " ++ show name)
  TEVar _ _ -> reject "module-qualified values"
  TEApply {} -> reject "function application"
  TELet {} -> reject "local bindings"
  TESequence {} -> reject "sequences"
  TEIf {} -> reject "conditionals"
  TEWhile {} -> reject "while loops"
  TEFor {} -> reject "for loops"
  TEVariant {} -> reject "variants"
  TEPolymorphicVariant {} -> reject "polymorphic variants"
  TETuple {} -> reject "tuples"
  TECons {} -> reject "lists"
  TEArray {} -> reject "arrays"
  TERecord {} -> reject "records"
  TEFieldAccess {} -> reject "record field access"
  TEFieldAssignment {} -> reject "record field assignment"
  TEMatch {} -> reject "pattern matching"
  TEFunction {} -> reject "function values"
  TETry {} -> reject "exception handling"

emitConstant :: Type -> Constant -> Emit Code
emitConstant ty constant = case constant of
  ConstantInt (IntLiteral number Unspecified) -> do
    let n = toInteger number
    when (n < negate (2 ^ (62 :: Int)) || n >= 2 ^ (62 :: Int)) $
      lift (Left ("Integer outside signed 63-bit range: " ++ show n))
    literal TInt 2 (show n ++ "L")
  ConstantInt _ -> reject "integer literals with an explicit size suffix"
  ConstantDouble number
    | isNaN number -> floatingDivision "dconst_0" "dconst_0"
    | isInfinite number -> do
        code <- floatingDivision "dconst_1" "dconst_0"
        pure (code <> if number < 0 then instruction 0 "dneg" else mempty)
    | otherwise -> literal TDouble 2 (show number)
  ConstantChar character
    | ord character <= 255 -> literal TChar 1 (show (ord character))
    | otherwise -> reject "character literals outside the OCaml byte range"
  ConstantString _ -> reject "string literals"
  ConstantBool boolean -> do
    requireType TBool ty
    pure (instruction 1 (if boolean then "iconst_1" else "iconst_0"))
  ConstantUnit -> requireType TUnit ty >> pure mempty
  ConstantList -> reject "list literals"
  ConstantArray -> reject "array literals"
  ConstantVariant _ -> reject "variant constants"
  ConstantPolymorphicVariant _ -> reject "polymorphic variant constants"
  where
    literal expected slots operand = do
      requireType expected ty
      let opcode = if slots == 2 then "ldc2_w " else "ldc "
      pure (instruction slots (opcode ++ operand))
    floatingDivision numerator denominator = do
      requireType TDouble ty
      pure (instruction 2 numerator <> instruction 2 denominator <> instruction (-2) "ddiv")
