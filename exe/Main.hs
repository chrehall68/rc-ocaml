module Main (main) where

import Frontend.Lexer (alexScanTokens)
import Frontend.Parser
import System.Exit (exitFailure)

failWith :: String -> IO a
failWith msg = do
  putStrLn msg
  exitFailure

orExit :: Either String a -> IO a
orExit (Left msg) = failWith msg
orExit (Right x) = return x

main :: IO ()
main = do
  source <- getContents
  tokens <- orExit $ alexScanTokens source
  print tokens
  ast <- orExit $ parse tokens
  print ast
