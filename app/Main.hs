module Main (main) where

f :: Int -> Int -> Int
f x y = x + y

main :: IO ()
main = do
  putStrLn "Enter two numbers:"
  x <- readLn :: IO Int
  y <- readLn :: IO Int
  print (f x y)
