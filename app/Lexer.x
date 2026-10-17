{
module Lexer  where
import Tokens
import Data.Array
import Data.Char
}

%wrapper "basic"

$digit = 0-9            -- digits

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
@base16Literal = \-? ['0x' '0X'] [$digit A-F a-f] [$digit A-F a-f \_]*
@base8Literal = \-? ['0o' '0O'] [0-7] [0-7 \_]*
@base2Literal = \-? ['0b' '0B'] [0-1] [0-1 \_]*
@integerLiteral = (@base10Literal | @base16Literal | @base8Literal | @base2Literal)
@int32Literal = @integerLiteral 'l'
@int64Literal = @integerLiteral 'L'
@nativeIntLiteral = @integerLiteral 'n' 

tokens :-
  $white+                             ;
  -- identifiers
  $upper $identifierSuffix*           { \s -> TokenCapitalizedIdentifier s }
  [$lower \_] $identifierSuffix*      { \s -> TokenLowercaseIdentifier s }
  -- integers
  @int32Literal                       { \s -> TokenInt (IntLiteral {value=(readOcamlInt (init s)), representation=Int32}) }
  @int64Literal                       { \s -> TokenInt (IntLiteral {value=(readOcamlInt (init s)), representation=Int64}) }
  @nativeIntLiteral                   { \s -> TokenInt (IntLiteral {value=(readOcamlInt (init s)), representation=NativeInt}) }
  @integerLiteral                     { \s -> TokenInt (IntLiteral {value=(readOcamlInt (init s)), representation=Unspecified}) }
  
    
