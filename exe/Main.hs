module Main (main) where

import Backend.Jasmin (generateJasmin)
import Data.Char (toUpper)
import Frontend.Lexer (alexScanTokens)
import Frontend.Parser (parse)
import Intermediate.MatchChecker (checkExhaustiveness)
import Intermediate.TypeAnnotator (lowerAndInfer)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitWith)
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (rawSystem)

failWith :: String -> IO a
failWith msg = do
  putStrLn msg
  exitFailure

orExit :: Either String a -> IO a
orExit (Left msg) = failWith msg
orExit (Right x) = return x

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> compileFile inputFile
    _ -> do
      prog <- getProgName
      hPutStrLn stderr $ "Usage: " ++ prog ++ " <input-file>"
      exitFailure

compileFile :: FilePath -> IO ()
compileFile inputFile = do
  let outputName = case takeBaseName inputFile of
        [] -> error "impossible"
        fstChar : rest -> toUpper fstChar : rest
  source <- readFile inputFile
  tokens <- orExit $ alexScanTokens source
  -- print tokens
  ast <- orExit $ parse tokens
  -- print ast
  typedAst <- orExit $ lowerAndInfer ast
  -- print typedAst
  matchCheckedAst <- orExit $ checkExhaustiveness typedAst
  jasminCode <- orExit $ generateJasmin outputName matchCheckedAst
  writeFile "out.j" jasminCode
  -- TODO: make sure we document that jasmin.jar and java need to be installed
  code <- rawSystem "java" ["-jar", "jasmin.jar", "out.j"]
  exitWith code
