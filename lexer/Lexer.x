{
module Lexer (alexScanTokens)  where
import Tokens
import Data.Array
import Data.Char
import Data.Maybe
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
  <0> {
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
    -- strings
    \{ [$lower \_]* \|                  { enterQuotedString }
    -- operators
    \-                                  { regularToken (const TokenMinus) }
    \+                                  { regularToken (const TokenPlus) }
    \*                                  { regularToken (const TokenStar) }
    \/                                  { regularToken (const TokenSlash) }
  }
  -- the rest of the machinery for recognizing quoted strings
  <quoted> {
    \| [$lower \_]* \}  /{ isEndOfQuotedString }   { exitQuotedString }
    [$white .]                                     { nextQuotedStringChar }
  }


{
-- our actual monad
data QuotedStringState = QuotedStringState {
    currentStringReversed :: String,
    delimiter :: String
}
data AlexUserState = AlexUserState {
    quotedString :: Maybe QuotedStringState
}
  

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState Nothing

enterQuotedString :: AlexAction Token
enterQuotedString (_,_,_,s) len = do
  let delim = tail (take (len-1) s)
      state = Just (QuotedStringState { currentStringReversed = "", delimiter = delim }) in
        alexSetUserState (AlexUserState { quotedString = state });
  alexSetStartCode quoted;
  scanToken

isEndOfQuotedString :: AlexUserState -> AlexInput -> Int -> AlexInput -> Bool
isEndOfQuotedString userState (_,_,_,s) len _ = do
  let stringState = fromJust (quotedString userState) in
    (tail (take (len-1) s)) == (delimiter stringState)

-- since we take one character at a time, we can just concatenate
nextQuotedStringChar :: AlexAction Token
nextQuotedStringChar (_,_,_,s) _ = do
  userState <- alexGetUserState;
  let stringState = fromJust (quotedString userState) in
    alexSetUserState (AlexUserState {
      quotedString = Just (stringState { currentStringReversed = (head s) : (currentStringReversed stringState) })
    });
  scanToken

exitQuotedString :: AlexAction Token
exitQuotedString _ _ = do
    userState <- alexGetUserState;
    alexSetUserState (AlexUserState Nothing);
    alexSetStartCode 0;
    let stringState = fromJust (quotedString userState) in
      return (TokenString (reverse (currentStringReversed stringState)))

regularToken :: (String -> Token) -> AlexAction Token
regularToken f =  \(_,_,_,s) len -> return (f (take len s))

alexEOF = do 
  userState <- alexGetUserState;
  case quotedString userState of
    Just _ -> alexError "unterminated string"
    Nothing -> return TokenEOF


-- similar to alexMonadScan, but calls alexScanUser instead of just alexScan
scanToken :: Alex Token
scanToken = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  us <- alexGetUserState
  case alexScanUser us inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line col), _, _, _) ->
      alexError ("lexical error at line " ++ show line ++ ", column " ++ show col)
    AlexSkip inp' _ -> alexSetInput inp' >> scanToken
    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) len

-- driver
alexScanTokens :: String -> Either String [Token]
alexScanTokens source = 
  let f = do
        tok <- scanToken
        case tok of
          TokenEOF -> return []
          _ -> (tok : ) <$> f
  in runAlex source f
}