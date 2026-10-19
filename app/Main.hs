module Main (main) where

import Lexer
import MyParser

main :: IO ()
main = do
  tokens <- getContents >>= return . alexScanTokens
  print tokens

-- ast <- return $ calc tokens
-- print ast
