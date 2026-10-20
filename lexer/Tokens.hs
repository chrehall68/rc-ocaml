module Tokens (Token (..), IntLiteral (..), IntRepresentation (..), decodeOcamlInt, decodeOcamlDouble, decodeChar) where

import Data.Char
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
  | TokenDouble Double
  | TokenChar Char
  | TokenString String
  | TokenMinus
  | TokenPlus
  | TokenStar
  | TokenSlash
  | TokenEOF
  deriving (Show)

removeUnderscore :: String -> String
removeUnderscore s = filter (/= '_') s

decodeOcamlInt :: String -> Int
decodeOcamlInt raw =
  case ( case removeUnderscore raw of
           '0' : 'x' : s -> readHex s
           '0' : 'X' : s -> readHex s
           '0' : 'o' : s -> readOct s
           '0' : 'O' : s -> readOct s
           '0' : 'b' : s -> readBin s
           '0' : 'B' : s -> readBin s
           s -> readDec s
       ) of
    [(i, "")] -> i
    _ -> error "invalid int literal"

decodeHexFloat :: String -> Double
decodeHexFloat s =
  let (mant, ex) = break (`elem` "pP") s
      (ip, fp) = break (== '.') mant
      frac = drop 1 fp
      digits = ip ++ frac
      m = fst (head (readHex digits)) :: Integer
      e = case ex of
        _ : '+' : d -> read d
        _ : d -> read d
        [] -> 0 :: Int
   in encodeFloat m (e - 4 * length frac)

decodeOcamlDouble :: String -> Double
decodeOcamlDouble raw =
  let s = removeUnderscore raw
   in case s of
        '0' : 'x' : rest -> decodeHexFloat rest
        _ -> read s

decodeChar :: String -> Char
decodeChar s =
  -- strip leading/trailing '
  let s' = tail (init s)
      first = head s'
      rest = tail s'
   in case first of
        '\\' -> case rest of
          -- simple escapes
          "\\" -> '\\'
          "\"" -> '\"'
          "\'" -> '\''
          "n" -> '\n'
          "t" -> '\t'
          "b" -> '\b'
          "r" -> '\r'
          -- the rest of the escape characters
          " " -> ' '
          -- unicode escapes
          'o' : _ -> chr (decodeOcamlInt ("0" ++ rest))
          'x' : _ -> chr (decodeOcamlInt ("0" ++ rest))
          -- digit
          _ -> chr (decodeOcamlInt rest)
        _ -> first
