module TokensSpec (spec) where

import Test.Hspec
import Tokens

spec :: Spec
spec = do
  describe "decodeOcamlInt" $ do
    it "decodes decimal" $ decodeOcamlInt "42" `shouldBe` 42
    it "ignores underscores" $ decodeOcamlInt "1_000_000" `shouldBe` 1000000
    it "decodes hex" $ do
      decodeOcamlInt "0xFF" `shouldBe` 255
      decodeOcamlInt "0Xff" `shouldBe` 255
    it "decodes octal" $ decodeOcamlInt "0o17" `shouldBe` 15
    it "decodes binary" $ decodeOcamlInt "0b101" `shouldBe` 5

  describe "decodeOcamlDouble" $ do
    it "decodes plain floats" $ decodeOcamlDouble "3.25" `shouldBe` 3.25
    it "decodes exponents" $ do
      decodeOcamlDouble "1e3" `shouldBe` 1000
      decodeOcamlDouble "2.5E-1" `shouldBe` 0.25
    it "ignores underscores" $ decodeOcamlDouble "1_0.5" `shouldBe` 10.5
    it "decodes a trailing dot" $ decodeOcamlDouble "1." `shouldBe` 1
    it "decodes a dot before an exponent" $ decodeOcamlDouble "1.e2" `shouldBe` 100
    it "decodes hex floats" $ do
      decodeOcamlDouble "0x1p3" `shouldBe` 8
      decodeOcamlDouble "0x1.p3" `shouldBe` 8
      decodeOcamlDouble "0x1.8p1" `shouldBe` 3

  describe "decodeChar" $ do
    it "decodes plain characters" $ decodeChar "a" `shouldBe` 'a'
    it "decodes simple escapes" $ do
      decodeChar "\\n" `shouldBe` '\n'
      decodeChar "\\t" `shouldBe` '\t'
      decodeChar "\\b" `shouldBe` '\b'
      decodeChar "\\r" `shouldBe` '\r'
      decodeChar "\\\\" `shouldBe` '\\'
      decodeChar "\\\"" `shouldBe` '"'
      decodeChar "\\'" `shouldBe` '\''
      decodeChar "\\ " `shouldBe` ' '
    it "decodes decimal escapes" $ decodeChar "\\065" `shouldBe` 'A'
    it "decodes octal escapes" $ decodeChar "\\o101" `shouldBe` 'A'
    it "decodes hex escapes" $ decodeChar "\\x41" `shouldBe` 'A'
