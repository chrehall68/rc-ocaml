module Backend.Jasmin (generateJasmin) where

import Control.Monad (foldM, unless, when)
import Control.Monad.State.Strict (StateT, get, lift, put, runStateT)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Frontend.Ast (Constant (..), Path (..))
import Frontend.Tokens (IntLiteral (IntLiteral), IntRepresentation (Unspecified))
import Intermediate.MatchChecker (ExhaustiveTProgram (..))
import Intermediate.Type (Type (..), builtinTypeName)
import Intermediate.TypedAst
import Numeric (showHex)
import Text.Read (readMaybe)

-- | Emit a Jasmin class with a static main method. The class name uses JVM
-- internal form (for example, "example/Program"). Start an unqualified
-- class name with an uppercase letter, '_' or '$' to avoid Jasmin keywords.
-- Top-level definitions run in order, and their bindings are local to main.
--
-- The backend supports primitive values, empty collections, variable and
-- wildcard bindings, control flow, and selected predefined functions. Other
-- constructs return an error until their JVM representations are defined.
generateJasmin :: String -> ExhaustiveTProgram -> Either String String
generateJasmin className (ExhaustiveTProgram (TProgram definitions)) = do
  unless (validClassName className) $ Left ("Invalid JVM class name " ++ show className)
  ((_, code), finalState) <- runStateT (foldM emitDefinition (Map.empty, mempty) definitions) (Supply 1 0)
  let locals = nextLocal finalState
  when (locals > 65535 || peak code > 65535) $ Left "JVM method exceeds the stack or locals limit"
  let body = render code ["  return"]
  validateBytecode body
  pure $
    unlines $
      [ ".class public " ++ className,
        ".super java/lang/Object",
        "",
        ".method public static main([Ljava/lang/String;)V",
        "  .limit stack " ++ show (peak code),
        "  .limit locals " ++ show locals
      ]
        ++ body
        ++ [".end method"]

validClassName :: String -> Bool
validClassName = validPart False
  where
    validPart qualified name = case break (== '/') name of
      ([], _) -> False
      (first : rest, suffix) ->
        identifierStart first
          && all (\c -> identifierStart c || isDigit c) rest
          && case suffix of
            [] -> qualified || not (isAsciiLower first)
            _ : remaining -> validPart True remaining
    identifierStart c = isAsciiLower c || isAsciiUpper c || c == '_' || c == '$'

-- JVM methods may contain at most 65,535 bytes of code, and the branches we
-- emit use signed 16-bit offsets. Use upper bounds for ldc and local access
-- sizes where Jasmin chooses the final instruction encoding.
validateBytecode :: [String] -> Either String ()
validateBytecode body = do
  sizes <- mapM bytecodeSize body
  let positions = init (scanl (+) 0 sizes)
      total = sum sizes
      targets = Map.fromList [(init line, offset) | (line, offset) <- zip body positions, ":" `isSuffixOf` line]
  when (total > 65535) $ Left "JVM method exceeds the 65,535-byte code limit"
  mapM_ (checkBranch targets) (zip body positions)
  where
    checkBranch targets (line, offset) = case words line of
      [op, target] | op `elem` branches -> case Map.lookup target targets of
        Nothing -> Left ("Missing JVM branch label " ++ target)
        Just destination ->
          when (destination - offset < -32768 || destination - offset > 32767) $
            Left ("JVM branch to " ++ target ++ " exceeds the 16-bit offset limit")
      _ -> Right ()

    branches = ["ifeq", "ifgt", "iflt", "goto"]

bytecodeSize :: String -> Either String Int
bytecodeSize line
  | ":" `isSuffixOf` line = Right 0
  | otherwise = case words line of
      [op] | op `elem` oneByte -> Right 1
      [op, _] | op `elem` ["ifeq", "ifgt", "iflt", "goto"] -> Right 3
      [op, slot] | op `elem` localAccess -> case readMaybe slot of
        Just n | n >= (0 :: Int) && n <= 65535 -> Right (if n <= 255 then 2 else 4)
        _ -> Left ("Invalid JVM local index " ++ slot)
      (op : _) | op `elem` ["ldc", "ldc2_w", "getstatic", "invokevirtual", "invokestatic", "anewarray"] -> Right 3
      _ -> Left ("Unknown JVM instruction size for " ++ line)
  where
    oneByte =
      [ "return",
        "pop",
        "pop2",
        "lcmp",
        "ladd",
        "lsub",
        "lmul",
        "ldiv",
        "dadd",
        "dsub",
        "dmul",
        "ddiv",
        "dneg",
        "l2d",
        "lshl",
        "lshr",
        "lconst_1",
        "dconst_0",
        "dconst_1",
        "iconst_0",
        "iconst_1",
        "aconst_null"
      ]
    localAccess = ["iload", "istore", "lload", "lstore", "dload", "dstore", "aload", "astore"]

-- Stack heights are measured in JVM slots. Double and long use two slots.
-- Each compiled expression leaves only its result above the incoming stack.
data Code = Code {delta :: Int, peak :: Int, render :: [String] -> [String]}

instance Semigroup Code where
  first <> second =
    Code
      (delta first + delta second)
      (max (peak first) (delta first + peak second))
      (render first . render second)

instance Monoid Code where
  mempty = Code 0 0 id

instruction :: Int -> String -> Code
instruction change name = Code change (max 0 change) (("  " ++ name) :)

label :: String -> Code
label name = Code 0 0 ((name ++ ":") :)

-- The first arm ends in a jump. The second starts at the same incoming
-- height as the first, and their result types agree at the join.
alternatives :: Code -> Code -> Code
alternatives first second =
  Code
    (delta second)
    (max (peak first) (peak second))
    (render first . render second)

data ValueKind = UnitValue | IntValue | LongValue | DoubleValue | RefValue

kind :: Type -> Emit ValueKind
kind ty = case ty of
  TUnit -> pure UnitValue
  TInt -> pure LongValue
  TBool -> pure IntValue
  TChar -> pure IntValue
  TFloat -> pure DoubleValue
  TString -> pure RefValue
  TCon name [_] | name == builtinTypeName "array" || name == builtinTypeName "list" -> pure RefValue
  _ -> reject ("JVM representation for " ++ show ty)

width :: ValueKind -> Int
width valueKind = case valueKind of
  UnitValue -> 0
  IntValue -> 1
  LongValue -> 2
  DoubleValue -> 2
  RefValue -> 1

data Local = Local Type ValueKind Int

type Env = Map.Map String Local

data Supply = Supply {nextLocal :: Int, nextLabel :: Int}

type Emit = StateT Supply (Either String)

reject :: String -> Emit a
reject feature = lift (Left ("Jasmin backend does not support " ++ feature))

requireType :: Type -> Type -> Emit ()
requireType expected actual =
  unless (expected == actual) $
    lift (Left ("Expected " ++ show expected ++ " but received " ++ show actual ++ " in lowered AST"))

-- Polymorphic list/array bindings can be instantiated at different element
-- types. Their element type is erased by the current JVM representation.
requireLocalType :: Type -> Type -> Emit ()
requireLocalType (TCon name1 [_]) (TCon name2 [_])
  | name1 == name2 && name1 `elem` [builtinTypeName "list", builtinTypeName "array"] = pure ()
requireLocalType expected actual = requireType expected actual

freshLocal :: Type -> Emit Local
freshLocal ty = do
  valueKind <- kind ty
  supply <- get
  let slot = nextLocal supply
  put supply {nextLocal = slot + width valueKind}
  pure (Local ty valueKind slot)

freshLabel :: Emit String
freshLabel = do
  supply <- get
  put supply {nextLabel = nextLabel supply + 1}
  pure ("L" ++ show (nextLabel supply))

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
        RefValue -> op 'a'

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
emitBindings env recursive bindings = do
  when recursive $ reject "recursive bindings"
  -- A parallel let group evaluates its RHSs in the original environment.
  foldM bind (env, mempty) bindings
  where
    bind (newEnv, code) (TLetBinding pat rhs) = case pat of
      TPVariable ty name -> do
        requireType ty (exprAnnotation rhs)
        rhsCode <- emitExpr env rhs
        local <- freshLocal ty
        pure (Map.insert name local newEnv, code <> rhsCode <> access "store" (-1) local)
      TPWildcard ty -> do
        requireType ty (exprAnnotation rhs)
        rhsCode <- emitDiscarded env rhs
        pure (newEnv, code <> rhsCode)
      _ -> reject "destructuring bindings"

emitDiscarded :: Env -> TExpr Type -> Emit Code
emitDiscarded env expression = do
  code <- emitExpr env expression
  valueKind <- kind (exprAnnotation expression)
  pure (code <> discard valueKind)

emitExpected :: Env -> Type -> TExpr Type -> Emit Code
emitExpected env ty expression = do
  requireType ty (exprAnnotation expression)
  emitExpr env expression

emitExpr :: Env -> TExpr Type -> Emit Code
emitExpr env expression = case expression of
  TEConstant ty constant -> emitConstant ty constant
  TEVar ty (Path Nothing name) -> case Map.lookup name env of
    Just local@(Local boundType _ _) -> do
      requireLocalType ty boundType
      pure (access "load" 1 local)
    Nothing -> reject ("unbound or first-class value " ++ show name)
  TEVar _ _ -> reject "module-qualified values"
  TEApply {} -> emitApply env expression
  TELet ty recursive bindings body -> do
    (newEnv, bindingsCode) <- emitBindings env recursive bindings
    bodyCode <- emitExpected newEnv ty body
    pure (bindingsCode <> bodyCode)
  TESequence ty first second ->
    (<>) <$> emitDiscarded env first <*> emitExpected env ty second
  TEIf ty condition yes no -> do
    elseLabel <- freshLabel
    endLabel <- freshLabel
    test <- emitExpected env TBool condition
    yesCode <- emitExpected env ty yes
    noCode <- emitExpected env ty no
    pure $
      test
        <> instruction (-1) ("ifeq " ++ elseLabel)
        <> alternatives (yesCode <> instruction 0 ("goto " ++ endLabel)) (label elseLabel <> noCode)
        <> label endLabel
  TEWhile ty condition body -> do
    requireType TUnit ty
    start <- freshLabel
    end <- freshLabel
    test <- emitExpected env TBool condition
    bodyCode <- emitExpected env TUnit body
    pure $
      label start
        <> test
        <> instruction (-1) ("ifeq " ++ end)
        <> bodyCode
        <> instruction 0 ("goto " ++ start)
        <> label end
  TEFor ty name first lastValue increasing body -> do
    requireType TUnit ty
    startCode <- emitExpected env TInt first
    endCode <- emitExpected env TInt lastValue
    counter <- freshLocal TInt
    bound <- freshLocal TInt
    start <- freshLabel
    end <- freshLabel
    bodyCode <- emitExpected (Map.insert name counter env) TUnit body
    let store = access "store" (-1)
        load = access "load" 1
        compareBounds = load counter <> load bound <> instruction (-3) "lcmp"
        outOfRange = if increasing then "ifgt " else "iflt "
        update = if increasing then "ladd" else "lsub"
    pure $
      startCode
        <> store counter
        <> endCode
        <> store bound
        <> compareBounds
        <> instruction (-1) (outOfRange ++ end)
        <> label start
        <> bodyCode
        <> compareBounds
        <> instruction (-1) ("ifeq " ++ end)
        <> load counter
        <> instruction 2 "lconst_1"
        <> instruction (-2) update
        <> store counter
        <> instruction 0 ("goto " ++ start)
        <> label end
  TEVariant {} -> reject "variants"
  -- TEPolymorphicVariant {} -> reject "polymorphic variants"
  TETuple {} -> reject "tuples"
  TEArray {} -> reject "nonempty arrays"
  TERecord {} -> reject "records"
  TEFieldAccess {} -> reject "record field access"
  TEFieldAssignment {} -> reject "record field assignment"
  TEMatch {} -> reject "pattern matching"
  TEFunction {} -> reject "function values"
  TETry {} -> reject "exception handling"

emitApply :: Env -> TExpr Type -> Emit Code
emitApply env application = case application of
  TEApply result (TEApply _ (TEVar _ (Path Nothing name)) left) right
    | Map.notMember name env,
      Just (ty, opcode) <- Map.lookup name arithmetic -> do
        requireType ty result
        leftCode <- emitExpected env ty left
        rightCode <- emitExpected env ty right
        pure (leftCode <> rightCode <> instruction (-2) opcode <> normalizeInt ty)
  TEApply result (TEVar _ (Path Nothing name)) argument
    | Map.notMember name env -> case name of
        "print_endline" -> do
          requireType TUnit result
          code <- emitExpected env TString argument
          pure $
            instruction 1 "getstatic java/lang/System/out Ljava/io/PrintStream;"
              <> code
              <> instruction (-2) "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"
        "string_of_int" -> conversion result argument TInt TString (-1) "invokestatic java/lang/Long/toString(J)Ljava/lang/String;"
        "string_of_bool" -> conversion result argument TBool TString 0 "invokestatic java/lang/Boolean/toString(Z)Ljava/lang/String;"
        "float_of_int" -> conversion result argument TInt TFloat 0 "l2d"
        _ -> reject ("application of " ++ show name)
  _ -> reject "general function application"
  where
    arithmetic =
      Map.fromList
        [ ("+", (TInt, "ladd")),
          ("-", (TInt, "lsub")),
          ("*", (TInt, "lmul")),
          ("/", (TInt, "ldiv")),
          ("+.", (TFloat, "dadd")),
          ("-.", (TFloat, "dsub")),
          ("*.", (TFloat, "dmul")),
          ("/.", (TFloat, "ddiv"))
        ]
    normalizeInt TInt =
      instruction 1 "iconst_1"
        <> instruction (-1) "lshl"
        <> instruction 1 "iconst_1"
        <> instruction (-1) "lshr"
    normalizeInt _ = mempty
    conversion result argument from to change opcode = do
      requireType to result
      code <- emitExpected env from argument
      pure (code <> instruction change opcode)

emitConstant :: Type -> TConstant -> Emit Code
emitConstant ty constant = case constant of
  TConstInt (IntLiteral number Unspecified) -> do
    let n = toInteger number
    when (n < negate (2 ^ (62 :: Int)) || n >= 2 ^ (62 :: Int)) $
      lift (Left ("Integer outside signed 63-bit range: " ++ show n))
    literal TInt 2 (show n) -- we don't actually need the L part
  TConstInt _ -> reject "integer literals with an explicit size suffix"
  TConstDouble number
    | isNaN number -> floatingDivision "dconst_0" "dconst_0"
    | isInfinite number -> do
        code <- floatingDivision "dconst_1" "dconst_0"
        pure (code <> if number < 0 then instruction 0 "dneg" else mempty)
    | otherwise -> literal TFloat 2 (show number)
  TConstChar character
    | ord character <= 255 -> literal TChar 1 (show (ord character))
    | otherwise -> reject "character literals outside the OCaml byte range"
  TConstString value -> do
    let (size, quoted) = jasminString value
    when (size > 65535) $ lift (Left "String literal exceeds the JVM constant pool entry limit")
    literal TString 1 quoted
  TConstBool boolean -> do
    requireType TBool ty
    pure (instruction 1 (if boolean then "iconst_1" else "iconst_0"))
  TConstUnit -> requireType TUnit ty >> pure mempty
  -- ConstantList -> case ty of
  --   TCon "list" [_] -> pure (instruction 1 "aconst_null")
  --   _ -> reject ("empty list with type " ++ show ty)
  TConstEmptyArray -> case ty of
    TCon name [_] | name == builtinTypeName "array" -> pure (instruction 1 "iconst_0" <> instruction 0 "anewarray java/lang/Object")
    _ -> reject ("empty array with type " ++ show ty)
  TConstVariant _ -> reject "variant constants"
  TConstPolymorphicVariant _ -> reject "polymorphic variant constants"
  where
    literal expected slots operand = do
      requireType expected ty
      let opcode = if slots == 2 then "ldc2_w " else "ldc "
      pure (instruction slots (opcode ++ operand))
    floatingDivision numerator denominator = do
      requireType TFloat ty
      pure (instruction 2 numerator <> instruction 2 denominator <> instruction (-2) "ddiv")

-- Jasmin reads Unicode escapes as UTF-16 code units. The size check uses
-- the modified UTF-8 byte count used by the JVM's CONSTANT_Utf8 entries.
jasminString :: String -> (Int, String)
jasminString value = (sum (map bytes value), '"' : concatMap escape value ++ "\"")
  where
    bytes c
      | ord c > 0xffff = 6
      | ord c > 0x7ff = 3
      | ord c > 0x7f || c == '\0' = 2
      | otherwise = 1
    escape c
      | c == '"' = "\\\""
      | c == '\\' = "\\\\"
      | c >= ' ' && c <= '~' = [c]
      | ord c <= 0xffff = unicode (ord c)
      | otherwise =
          let (high, low) = (ord c - 0x10000) `divMod` 0x400
           in unicode (0xd800 + high) ++ unicode (0xdc00 + low)
    unicode n = let digits = showHex n "" in "\\u" ++ replicate (4 - length digits) '0' ++ digits
