module Main (main) where

import Lexer
import MyParser

f :: Int -> Int -> Int
f x y = x + y

main :: IO ()
main = do
  putStrLn "Enter two numbers:"
  x <- readLn :: IO Int
  y <- readLn :: IO Int
  print (f x y)
  tokens <- getContents >>= return . alexScanTokens
  print tokens
  ast <- return $ calc tokens
  print ast
