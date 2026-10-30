module LexerSpec (spec) where

import Control.Monad (forM_)
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
      alexScanTokens "foo Bar x' y_1"
        `shouldBe` Right
          [ TokenLowercaseIdentifier "foo",
            TokenCapitalizedIdentifier "Bar",
            TokenLowercaseIdentifier "x'",
            TokenLowercaseIdentifier "y_1"
          ]
    it "lexes a lone underscore as the wildcard" $
      alexScanTokens "_ _a" `shouldBe` Right [TokenUnderscore, TokenLowercaseIdentifier "_a"]

    it "lexes integer literals" $
      alexScanTokens "1 0xff 0o7 0b1 1_0"
        `shouldBe` Right
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
      alexScanTokens "+ * ( ) , ;"
        `shouldBe` Right
          [TokenPlus, TokenStar, TokenLParen, TokenRParen, TokenComma, TokenSemi]
    it "uses the longest match for multi-character symbols" $
      alexScanTokens ":: := :> -> -. <- ;; != && || [| |] .. .~"
        `shouldBe` Right
          [ TokenColonColon,
            TokenColonEq,
            TokenColonGt,
            TokenArrow,
            TokenMinusDot,
            TokenLeftArrow,
            TokenSemiSemi,
            TokenPhysicalNE,
            TokenAmpersandAmpersand,
            TokenPipePipe,
            TokenLBracketPipe,
            TokenPipeRBracket,
            TokenDotDot,
            TokenDotTilde
          ]

    it "lexes a small expression" $
      alexScanTokens "let x = 1 in x + 2"
        `shouldBe` Right
          [ TokenLet,
            TokenLowercaseIdentifier "x",
            TokenEq,
            int 1 Unspecified,
            TokenIn,
            TokenLowercaseIdentifier "x",
            TokenPlus,
            int 2 Unspecified
          ]

    it "reports a lexical error with its position" $
      alexScanTokens "let \x01" `shouldSatisfy` isLeft
  describe "keyword symbols" $ do
    forM_
      [ ("!=", TokenPhysicalNE),
        ("&", TokenAmpersand),
        ("&&", TokenAmpersandAmpersand),
        ("*", TokenStar),
        ("+", TokenPlus),
        ("-", TokenMinus),
        ("-.", TokenMinusDot),
        ("->", TokenArrow),
        (".", TokenDot),
        ("..", TokenDotDot),
        (".~", TokenDotTilde),
        (":", TokenColon),
        ("::", TokenColonColon),
        (":=", TokenColonEq),
        (":>", TokenColonGt),
        (";", TokenSemi),
        (";;", TokenSemiSemi),
        ("<", TokenLt),
        ("<-", TokenLeftArrow),
        ("=", TokenEq),
        (">", TokenGt),
        ("?", TokenQuestion),
        ("|", TokenPipe),
        ("||", TokenPipePipe),
        ("~", TokenTilde),
        ("#", TokenHash)
      ]
      $ \(text, token) ->
        it ("lexes " ++ text ++ " as a keyword symbol") $
          alexScanTokens text `shouldBe` Right [token]
    it "keeps keyword symbols apart from their neighbours when spaced" $
      alexScanTokens "x := y -> z"
        `shouldBe` Right
          [ TokenLowercaseIdentifier "x",
            TokenColonEq,
            TokenLowercaseIdentifier "y",
            TokenArrow,
            TokenLowercaseIdentifier "z"
          ]
    it "lexes a keyword symbol directly against identifiers" $
      alexScanTokens "a<-b"
        `shouldBe` Right
          [TokenLowercaseIdentifier "a", TokenLeftArrow, TokenLowercaseIdentifier "b"]

  describe "infix symbols" $ do
    forM_
      ["<=", ">=", "+=", "|>", "<|", "**", "@@", "^^", "==", "<>", "+.", "*.", "->>", "<--", "&&&", "|||", "#+", "$", "%", "/", "@", "^", "-->"]
      $ \text ->
        it ("lexes " ++ text ++ " as a single infix symbol") $
          alexScanTokens text `shouldBe` Right [TokenInfix text]
    it "does not confuse infix symbols with the keyword symbols they start with" $ do
      alexScanTokens "|| |||" `shouldBe` Right [TokenPipePipe, TokenInfix "|||"]
      alexScanTokens "& &&&" `shouldBe` Right [TokenAmpersand, TokenInfix "&&&"]
      alexScanTokens "- -> ->>" `shouldBe` Right [TokenMinus, TokenArrow, TokenInfix "->>"]
      alexScanTokens "= ==" `shouldBe` Right [TokenEq, TokenInfix "=="]
    it "splits infix symbols at whitespace" $
      alexScanTokens "< =" `shouldBe` Right [TokenLt, TokenEq]
    it "lexes infix symbols between operands without spaces" $
      alexScanTokens "a<=b"
        `shouldBe` Right
          [TokenLowercaseIdentifier "a", TokenInfix "<=", TokenLowercaseIdentifier "b"]
    it "lets a leading # start an infix symbol only when operator characters follow" $ do
      alexScanTokens "#x" `shouldBe` Right [TokenHash, TokenLowercaseIdentifier "x"]
      alexScanTokens "#<" `shouldBe` Right [TokenInfix "#<"]

  describe "prefix symbols" $ do
    forM_ ["!", "!!", "!==", "!+", "?+", "?!", "~+", "~~"] $ \text ->
      it ("lexes " ++ text ++ " as a single prefix symbol") $
        alexScanTokens text `shouldBe` Right [TokenPrefix text]
    it "lexes a dereference" $
      alexScanTokens "!x" `shouldBe` Right [TokenPrefix "!", TokenLowercaseIdentifier "x"]
    it "prefers != over the prefix symbol" $
      alexScanTokens "a != b"
        `shouldBe` Right
          [TokenLowercaseIdentifier "a", TokenPhysicalNE, TokenLowercaseIdentifier "b"]
    it "leaves a lone ? or ~ before an identifier as a keyword symbol" $ do
      alexScanTokens "?x" `shouldBe` Right [TokenQuestion, TokenLowercaseIdentifier "x"]
      alexScanTokens "~x" `shouldBe` Right [TokenTilde, TokenLowercaseIdentifier "x"]
    it "lexes labels" $ do
      alexScanTokens "~x:" `shouldBe` Right [TokenLabel "x"]
      alexScanTokens "?x:" `shouldBe` Right [TokenOptionalLabel "x"]
    it "mixes prefix and infix symbols" $
      alexScanTokens "x := !y + 1"
        `shouldBe` Right
          [ TokenLowercaseIdentifier "x",
            TokenColonEq,
            TokenPrefix "!",
            TokenLowercaseIdentifier "y",
            TokenPlus,
            int 1 Unspecified
          ]

  describe "whitespace" $ do
    it "skips every kind of ASCII whitespace" $
      alexScanTokens " \t\r\n\f\v x" `shouldBe` Right [TokenLowercaseIdentifier "x"]
    it "separates tokens across newlines" $ do
      alexScanTokens "x\ny" `shouldBe` Right [TokenLowercaseIdentifier "x", TokenLowercaseIdentifier "y"]
      alexScanTokens "x\r\ny" `shouldBe` Right [TokenLowercaseIdentifier "x", TokenLowercaseIdentifier "y"]
      alexScanTokens "\n\n\nlet\n\n" `shouldBe` Right [TokenLet]
    it "separates symbols across newlines" $
      alexScanTokens "<\n=" `shouldBe` Right [TokenLt, TokenEq]
    it "keeps newlines inside quoted strings" $
      alexScanTokens "\"a\nb\r\nc\"" `shouldBe` Right [TokenString "a\nb\r\nc"]
    it "keeps newlines inside delimited strings" $
      alexScanTokens "{|a\nb|}" `shouldBe` Right [TokenString "a\nb"]
    it "keeps whitespace inside strings" $
      alexScanTokens "\"  \t \"" `shouldBe` Right [TokenString "  \t "]
    it "counts lines in error positions" $ do
      alexScanTokens "x\n\x01" `shouldBe` Left "Unexpected token at line 2, column 1"
      alexScanTokens "x\n\ny \x01" `shouldBe` Left "Unexpected token at line 3, column 3"
    it "rejects unicode whitespace outside strings, like OCaml" $ do
      alexScanTokens "x\x00a0y" `shouldSatisfy` isLeft
      alexScanTokens "let\x2003x" `shouldSatisfy` isLeft

  describe "unicode" $ do
    it "lexes latin-1 letters in identifiers" $ do
      alexScanTokens "é ñandú"
        `shouldBe` Right
          [TokenLowercaseIdentifier "é", TokenLowercaseIdentifier "ñandú"]
      alexScanTokens "École" `shouldBe` Right [TokenCapitalizedIdentifier "École"]
    it "accepts any unicode inside quoted strings" $
      alexScanTokens "\"é日本😀\"" `shouldBe` Right [TokenString "é日本😀"]
    it "accepts any unicode inside delimited strings" $
      alexScanTokens "{|é日本😀|}" `shouldBe` Right [TokenString "é日本😀"]
    it "decodes \\u{...} escapes in quoted strings" $ do
      alexScanTokens "\"\\u{41}\"" `shouldBe` Right [TokenString "A"]
      alexScanTokens "\"\\u{1F600}\"" `shouldBe` Right [TokenString "😀"]
    it "lexes a latin-1 character literal" $
      alexScanTokens "'é'" `shouldBe` Right [TokenChar 'é']
    it "rejects non-latin letters outside strings" $ do
      alexScanTokens "λ" `shouldSatisfy` isLeft
      alexScanTokens "日本" `shouldSatisfy` isLeft
    it "tracks columns in characters, not bytes" $
      alexScanTokens "é \x01" `shouldBe` Left "Unexpected token at line 1, column 3"
  where
    isLeft (Left _) = True
    isLeft _ = False
