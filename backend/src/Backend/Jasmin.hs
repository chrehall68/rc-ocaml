module Backend.Jasmin (generateJasmin) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Intermediate.Type (Type)
import Intermediate.TypedAst (TProgram (..))

-- | Emit a Jasmin class with a static main method.
generateJasmin :: String -> TProgram Type -> Either String String
generateJasmin className (TProgram definitions)
  | not (validClassName className) = Left ("Invalid JVM class name " ++ show className)
  | not (null definitions) = Left "Jasmin backend only supports empty programs"
  | otherwise = Right $ unlines
      [ ".class public " ++ className,
        ".super java/lang/Object",
        "",
        ".method public static main([Ljava/lang/String;)V",
        "  .limit stack 0",
        "  .limit locals 1",
        "  return",
        ".end method"
      ]

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
