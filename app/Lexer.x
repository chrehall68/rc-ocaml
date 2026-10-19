{
module Lexer  where
import Tokens
import Data.Array
import Data.Char
}

%wrapper "basic"

-- digit building blocks
$digit = 0-9            
$octalDigit = [0-7]
$hexDigit = [$digit A-F a-f]

-- identifiers
$lower =
    [a-z ß-ö ø-ÿ œ š ž]

$upper =
    [A-Z À-Ö Ø-Þ Œ Š Ž Ÿ ẞ]

$letter = [$lower $upper]
$identifierSuffix = [$letter $digit \_ \']
$ident = [$letter \_] 
 
-- integers
@base10Literal = \-? $digit [ $digit \_ ]*
@base16Literal = \-? 0 [xX] $hexDigit [$hexDigit \_]*
@base8Literal = \-? 0 [oO] [0-7] [0-7 \_]*
@base2Literal = \-? 0 [bB] [0-1] [0-1 \_]*
@integerLiteral = (@base16Literal | @base8Literal | @base2Literal | @base10Literal)
@int32Literal = @integerLiteral l
@int64Literal = @integerLiteral L
@nativeIntLiteral = @integerLiteral n 
-- floats
@floatBase10 = \-? $digit [$digit \_]* (\. [$digit \_]*)? ([e E] [\+ \-]? $digit [$digit \_]*)?
@floatBase16 = \-? 0 [xX] $hexDigit [$hexDigit \_]* (\. [$hexDigit \_]*)? ([p P] [\+ \-]? $digit [$digit \_]*)?
-- characters
$simpleEscape = [\\ \" \' n t b r]
@escape = \\ ( $simpleEscape | " " | $digit{3} | o $octalDigit{3} | x $hexDigit{2} )
@regularChar = [^ \\ \' \n \r]


tokens :-
  $white+                             ;
  -- identifiers
  $upper $identifierSuffix*           { \s -> TokenCapitalizedIdentifier s }
  [$lower \_] $identifierSuffix*      { \s -> TokenLowercaseIdentifier s }
  -- integers
  @int32Literal                       { \s -> TokenInt (IntLiteral {value=(decodeOcamlInt s), representation=Int32}) }
  @int64Literal                       { \s -> TokenInt (IntLiteral {value=(decodeOcamlInt s), representation=Int64}) }
  @nativeIntLiteral                   { \s -> TokenInt (IntLiteral {value=(decodeOcamlInt s), representation=NativeInt}) }
  @integerLiteral                     { \s -> TokenInt (IntLiteral {value=(decodeOcamlInt s), representation=Unspecified}) }
  -- floats
  @floatBase10                        { \s -> TokenDouble (decodeOcamlDouble s)}
  @floatBase16                        { \s -> TokenDouble (decodeOcamlDouble s)}
  -- characters
  \' (@regularChar | @escape) \'        { \s -> TokenChar (decodeChar s) }

