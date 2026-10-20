module Main (main) where

import Lexer (alexScanTokens)
import MyParser

main :: IO ()
main = do
  tokens <-
    getContents
      >>= ( \source -> case alexScanTokens source of
              Left err -> error err
              Right tokens -> return tokens
          )
  print tokens

-- ast <- return $ calc tokens
-- print ast
