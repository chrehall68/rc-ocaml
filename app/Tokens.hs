module Tokens (Token (..), IntLiteral (..), IntRepresentation (..), readOcamlInt) where

import Numeric

data IntRepresentation
  = Unspecified
  | Int32
  | Int64
  | NativeInt
  deriving (Show)

data IntLiteral = IntLiteral
  { value :: Int,
    representation :: IntRepresentation
  }
  deriving (Show)

data Token
  = TokenCapitalizedIdentifier String
  | TokenLowercaseIdentifier String
  | TokenInt IntLiteral
  deriving (Show)

removeUnderscore :: String -> String
removeUnderscore s = filter (/= '_') s

readOcamlInt :: String -> Int
readOcamlInt raw =
  let s = removeUnderscore raw
   in case readDec s of
        [(x, "")] -> x
        _ ->
          case readOct s of
            [(x, "")] -> x
            _ -> case readHex s of
              [(x, "")] -> x
              _ -> case readBin s of
                [(x, "")] -> x
                _ -> error "Invalid integer literal"