module LexerSpec (spec) where

import Lexer (alexScanTokens)
import Test.Hspec
import Tokens

int :: Int -> IntRepresentation -> Token
int v r = TokenInt (IntLiteral {value = v, representation = r})

spec :: Spec
spec = do
  describe "alexScanTokens" $ do
    it "lexes the empty string" $ alexScanTokens "" `shouldBe` Right []
    it "skips whitespace" $ alexScanTokens " \n\t " `shouldBe` Right []

    it "lexes keywords" $
      alexScanTokens "let rec in" `shouldBe` Right [TokenLet, TokenRec, TokenIn]
    it "prefers identifiers over keyword prefixes" $
      alexScanTokens "letter" `shouldBe` Right [TokenLowercaseIdentifier "letter"]

    it "lexes identifiers" $
      alexScanTokens "foo Bar x' y_1" `shouldBe` Right
        [ TokenLowercaseIdentifier "foo",
          TokenCapitalizedIdentifier "Bar",
          TokenLowercaseIdentifier "x'",
          TokenLowercaseIdentifier "y_1"
        ]
    it "lexes a lone underscore as the wildcard" $
      alexScanTokens "_ _a" `shouldBe` Right [TokenUnderscore, TokenLowercaseIdentifier "_a"]

    it "lexes integer literals" $
      alexScanTokens "1 0xff 0o7 0b1 1_0" `shouldBe` Right
        [ int 1 Unspecified,
          int 255 Unspecified,
          int 7 Unspecified,
          int 1 Unspecified,
          int 10 Unspecified
        ]
    it "lexes suffixed integer literals" $
      alexScanTokens "5l 5L 5n" `shouldBe` Right [int 5 Int32, int 5 Int64, int 5 NativeInt]

    it "lexes floats" $
      alexScanTokens "1.5 2e3 0x1p3" `shouldBe` Right [TokenDouble 1.5, TokenDouble 2000, TokenDouble 8]

    it "lexes characters" $
      alexScanTokens "'a' '\\n' '\\x41'" `shouldBe` Right [TokenChar 'a', TokenChar '\n', TokenChar 'A']

    it "lexes quoted strings" $
      alexScanTokens "\"hi\\n there\"" `shouldBe` Right [TokenString "hi\n there"]
    it "skips a backslash-newline and the indentation after it" $
      alexScanTokens "\"ab\\\n   cd\"" `shouldBe` Right [TokenString "abcd"]
    it "lexes delimited strings" $ do
      alexScanTokens "{|a\\nb|}" `shouldBe` Right [TokenString "a\\nb"]
      alexScanTokens "{id|a |} b|id}" `shouldBe` Right [TokenString "a |} b"]
    it "rejects unterminated strings" $ do
      alexScanTokens "\"abc" `shouldSatisfy` isLeft
      alexScanTokens "{|abc" `shouldSatisfy` isLeft

    it "lexes single-character symbols" $
      alexScanTokens "+ * ( ) , ;" `shouldBe` Right
        [TokenPlus, TokenStar, TokenLParen, TokenRParen, TokenComma, TokenSemi]
    it "uses the longest match for multi-character symbols" $
      alexScanTokens ":: := :> -> -. <- ;; != && || [| |] .. .~" `shouldBe` Right
        [ TokenColonColon, TokenColonEq, TokenColonGt, TokenArrow, TokenMinusDot,
          TokenLeftArrow, TokenSemiSemi, TokenPhysicalNE, TokenAmpersandAmpersand,
          TokenPipePipe, TokenLBracketPipe, TokenPipeRBracket, TokenDotDot, TokenDotTilde
        ]

    it "lexes a small expression" $
      alexScanTokens "let x = 1 in x + 2" `shouldBe` Right
        [ TokenLet, TokenLowercaseIdentifier "x", TokenEq, int 1 Unspecified, TokenIn,
          TokenLowercaseIdentifier "x", TokenPlus, int 2 Unspecified
        ]

    it "reports a lexical error with its position" $
      alexScanTokens "let \x01" `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
