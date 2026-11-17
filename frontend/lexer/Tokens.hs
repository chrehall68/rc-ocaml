module Tokens (Token (..), IntLiteral (..), IntRepresentation (..), infixToken, decodeOcamlInt, decodeOcamlDouble, decodeChar, decodeUnicodeChar) where

import Data.Char
import Numeric

data IntRepresentation
  = Unspecified
  | Int32
  | Int64
  | NativeInt
  deriving (Show, Eq)

data IntLiteral = IntLiteral
  { value :: Int,
    representation :: IntRepresentation
  }
  deriving (Show, Eq)

data Token
  = TokenCapitalizedIdentifier String
  | TokenLowercaseIdentifier String
  | TokenInt IntLiteral
  | TokenDouble Double
  | TokenChar Char
  | TokenString String -- string is the contents of the string, without quotes/delimiters
  | TokenLabel String -- string is just the label name
  | TokenOptionalLabel String -- string is just the label name
  | -- infix symbols are split by precedence class (see infixToken); the string is the full operator
    TokenInfix0 String -- =... <... >... |... &... $...
  | TokenInfix1 String -- @... ^...
  | TokenInfix2 String -- +... -...
  | TokenInfix3 String
  | -- \*... /... %...
    TokenInfix4 String
  | -- \**...
    TokenHashOp String -- #...
  | TokenPrefix String
  | -- keywords
    TokenAnd
  | TokenAs
  | TokenAssert
  | TokenAsr
  | TokenBegin
  | TokenClass
  | TokenConstraint
  | TokenDo
  | TokenDone
  | TokenDownto
  | TokenElse
  | TokenEnd
  | TokenException
  | TokenExternal
  | TokenFalse
  | TokenFor
  | TokenFun
  | TokenFunction
  | TokenFunctor
  | TokenIf
  | TokenIn
  | TokenInclude
  | TokenInherit
  | TokenInitializer
  | TokenLand
  | TokenLazy
  | TokenLet
  | TokenLor
  | TokenLsl
  | TokenLsr
  | TokenLxor
  | TokenMatch
  | TokenMethod
  | TokenMod
  | TokenModule
  | TokenMutable
  | TokenNew
  | TokenNonrec
  | TokenObject
  | TokenOf
  | TokenOpen
  | TokenOr
  | TokenPrivate
  | TokenRec
  | TokenSig
  | TokenStruct
  | TokenThen
  | TokenTo
  | TokenTrue
  | TokenTry
  | TokenType
  | TokenVal
  | TokenVirtual
  | TokenWhen
  | TokenWhile
  | TokenWith
  | -- keyword symbols
    TokenPhysicalNE
  | TokenHash
  | TokenAmpersand
  | TokenAmpersandAmpersand
  | TokenQuote -- single quote
  | TokenLParen
  | TokenRParen
  | TokenStar
  | TokenPlus
  | TokenComma
  | TokenMinus
  | TokenMinusDot
  | TokenArrow
  | TokenDot
  | TokenDotDot
  | TokenDotTilde
  | TokenColon
  | TokenColonColon
  | TokenColonEq
  | TokenColonGt
  | TokenSemi
  | TokenSemiSemi
  | TokenLt
  | TokenLeftArrow
  | TokenEq
  | TokenGt
  | TokenGtRBracket
  | TokenGtRBrace
  | TokenQuestion
  | TokenLBracket
  | TokenLBracketLt
  | TokenLBracketGt
  | TokenLBracketPipe
  | TokenRBracket
  | TokenUnderscore
  | TokenBacktick
  | TokenLBrace
  | TokenLBraceLt
  | TokenPipe
  | TokenPipeRBracket
  | TokenPipePipe
  | TokenRBrace
  | TokenTilde
  | TokenEOF
  deriving (Show, Eq)

-- classifies an infix symbol by its precedence level, as in
-- https://ocaml.org/manual/5.3/expr.html#ss:precedence-and-associativity
-- this makes the parser's job of assigning precedence possible
infixToken :: String -> Token
infixToken s = case s of
  '#' : _ -> TokenHashOp s
  '*' : '*' : _ -> TokenInfix4 s
  c : _
    | c `elem` "*/%" -> TokenInfix3 s
    | c `elem` "+-" -> TokenInfix2 s
    | c `elem` "@^" -> TokenInfix1 s
  _ -> TokenInfix0 s

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
        _ -> read (padTrailingDot s)

-- `read` requires that the . is followed by at least one digit
padTrailingDot :: String -> String
padTrailingDot ('.' : c : cs) | not (isDigit c) = '.' : '0' : c : padTrailingDot cs
padTrailingDot ['.'] = ".0"
padTrailingDot (c : cs) = c : padTrailingDot cs
padTrailingDot [] = []

-- s should not have the leading or trailing '
decodeChar :: String -> Char
decodeChar s =
  let first = head s
      rest = tail s
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

-- \u{...}
decodeUnicodeChar :: String -> Char
decodeUnicodeChar s = chr (decodeOcamlInt ("0x" ++ (drop 3 (init s))))
