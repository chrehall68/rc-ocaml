{
module Lexer (alexScanTokens)  where
import Tokens
import Data.Array
import Data.Char

}

%wrapper "monadUserState"

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
@base10Literal = $digit [ $digit \_ ]*
@base16Literal = 0 [xX] $hexDigit [$hexDigit \_]*
@base8Literal = 0 [oO] [0-7] [0-7 \_]*
@base2Literal = 0 [bB] [0-1] [0-1 \_]*
@integerLiteral = (@base16Literal | @base8Literal | @base2Literal | @base10Literal)
@int32Literal = @integerLiteral l
@int64Literal = @integerLiteral L
@nativeIntLiteral = @integerLiteral n 
-- floats
@floatBase10 = $digit [$digit \_]* (\. [$digit \_]*)? ([e E] [\+ \-]? $digit [$digit \_]*)?
@floatBase16 = 0 [xX] $hexDigit [$hexDigit \_]* (\. [$hexDigit \_]*)? ([p P] [\+ \-]? $digit [$digit \_]*)?
-- characters
$simpleEscape = [\\ \" \' n t b r]
@escape = \\ ( $simpleEscape | " " | $digit{3} | o $octalDigit{3} | x $hexDigit{2} )
@regularChar = [^ \\ \' \n \r]


tokens :-
  $white+                             ;
  -- identifiers
  $upper $identifierSuffix*           { regularToken (\s -> TokenCapitalizedIdentifier s) }
  [$lower \_] $identifierSuffix*      { regularToken (\s -> TokenLowercaseIdentifier s) }
  -- integers
  @int32Literal                       { regularToken (\s -> TokenInt (IntLiteral {value=(decodeOcamlInt s), representation=Int32}) ) }
  @int64Literal                       { regularToken (\s -> TokenInt (IntLiteral {value=(decodeOcamlInt s), representation=Int64}) ) }
  @nativeIntLiteral                   { regularToken (\s -> TokenInt (IntLiteral {value=(decodeOcamlInt s), representation=NativeInt}) ) }
  @integerLiteral                     { regularToken (\s -> TokenInt (IntLiteral {value=(decodeOcamlInt s), representation=Unspecified}) ) }
  -- floats
  @floatBase10                        { regularToken (\s -> TokenDouble (decodeOcamlDouble s) )}
  @floatBase16                        { regularToken (\s -> TokenDouble (decodeOcamlDouble s) )}
  -- characters
  \' (@regularChar | @escape) \'      { regularToken (\s -> TokenChar (decodeChar s) ) }
  -- operators
  \-                                  { regularToken (const TokenMinus) }
  \+                                  { regularToken (const TokenPlus) }
  \*                                  { regularToken (const TokenStar) }
  \/                                  { regularToken (const TokenSlash) }


{
-- our actual monad
data AlexUserState = AlexUserState
  { test:: String}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState []

regularToken :: (String -> Token) -> AlexAction Token
regularToken f =  \(_,_,_,s) len -> return (f (take len s))

-- TODO - maybe change
alexEOF = return TokenEOF


-- driver
alexScanTokens :: String -> Either String [Token]
alexScanTokens source = 
  let f = do
        tok <- alexMonadScan
        case tok of
          TokenEOF -> return []
          _ -> (tok : ) <$> f
  in runAlex source f
}