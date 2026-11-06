module Backend.Jasmin (generateJasmin) where

import Control.Monad (foldM, unless, when)
import Control.Monad.State.Strict (StateT, lift, runStateT)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Intermediate.Type (Type)
import Intermediate.TypedAst

-- | Emit a Jasmin class with a static main method.
-- The class name uses JVM internal form, for example "example/Program".
-- Unsupported typed AST nodes return an error.
generateJasmin :: String -> TProgram Type -> Either String String
generateJasmin className (TProgram definitions) = do
  unless (validClassName className) $ Left ("Invalid JVM class name " ++ show className)
  ((_, code), finalState) <- runStateT (foldM emitDefinition ((), mempty) definitions) (Supply 1)
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

type Env = ()

newtype Supply = Supply {nextLocal :: Int}
type Emit = StateT Supply (Either String)

reject :: String -> Emit a
reject feature = lift (Left ("Jasmin backend does not support " ++ feature))

emitDefinition :: (Env, Code) -> TDefinition Type -> Emit (Env, Code)
emitDefinition (env, code) definition = case definition of
  TDefinitionType _ -> pure (env, code)
  _ -> reject "runtime definitions"
