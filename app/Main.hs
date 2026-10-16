module Main (main) where

import MyParser

f :: Int -> Int -> Int
f x y = x + y

main :: IO ()
main = do
  putStrLn "Enter two numbers:"
  x <- readLn :: IO Int
  y <- readLn :: IO Int
  print (f x y)
  tokens <- getContents >>= return . lexer  
  print tokens
  ast <- return $ calc tokens
  print ast
