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
    \' (@regularChar | @escape) \'      { regularToken (\s -> TokenChar (decodeChar (tail (init s)))) }
    -- strings
    \"                                  { enterQuotedString }
    \{ [$lower \_]* \|                  { enterDelimitedString }
    -- operators
    \-                                  { regularToken (const TokenMinus) }
    \+                                  { regularToken (const TokenPlus) }
    \*                                  { regularToken (const TokenStar) }
    \/                                  { regularToken (const TokenSlash) }
  }
  -- the rest of the machinery for recognizing strings
  <delimited> {
    \| [$lower \_]* \}  /{ isEndOfDelimitedString }   { exitDelimitedString }
    [$white .]                                        { nextDelimitedStringChar }
  }
  <quoted> {
    \"                                                          { exitQuotedString }
    (@escape | $white | [^ \\])                                 { nextQuotedStringChar }
    \\ $white+                                                  ;  -- ignore whitespace following backslash
  }

{
-- our actual monad
data DelimitedStringState = DelimitedStringState {
    delimitedReversed :: String,
    delimiter :: String
}
data QuotedStringState = QuotedStringState {
    quotedReversed :: String
}
data AlexUserState = AlexUserState {
    delimitedString :: Maybe DelimitedStringState,
    quotedString :: Maybe QuotedStringState
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState Nothing Nothing

-- delimited strings
enterDelimitedString :: AlexAction Token
enterDelimitedString (_,_,_,s) len = do
  userState <- alexGetUserState;
  let delim = tail (take (len-1) s)
      state = Just (DelimitedStringState { delimitedReversed = "", delimiter = delim }) in
        alexSetUserState (userState { delimitedString = state });
  alexSetStartCode delimited;
  scanToken

isEndOfDelimitedString :: AlexUserState -> AlexInput -> Int -> AlexInput -> Bool
isEndOfDelimitedString userState (_,_,_,s) len _ = do
  let stringState = fromJust (delimitedString userState) in
    (tail (take (len-1) s)) == (delimiter stringState)

-- since we take one character at a time, we can just concatenate
nextDelimitedStringChar :: AlexAction Token
nextDelimitedStringChar (_,_,_,s) _ = do
  userState <- alexGetUserState;
  let stringState = fromJust (delimitedString userState) in
    alexSetUserState (userState {
      delimitedString = Just (stringState { delimitedReversed = (head s) : (delimitedReversed stringState) })
    });
  scanToken

exitDelimitedString :: AlexAction Token
exitDelimitedString _ _ = do
    userState <- alexGetUserState;
    alexSetUserState (userState { delimitedString = Nothing });
    alexSetStartCode 0;
    let stringState = fromJust (delimitedString userState) in
      return (TokenString (reverse (delimitedReversed stringState)))

-- quoted strings
enterQuotedString :: AlexAction Token
enterQuotedString (_,_,_,s) len = do
  userState <- alexGetUserState;
  let state = Just (QuotedStringState { quotedReversed = "" }) in
    alexSetUserState (userState { quotedString = state });
  alexSetStartCode quoted;
  scanToken

nextQuotedStringChar :: AlexAction Token
nextQuotedStringChar (_,_,_,s) len = do
  userState <- alexGetUserState;
  let stringState = fromJust (quotedString userState) in
    alexSetUserState (userState {
      quotedString = Just (stringState { quotedReversed = (decodeChar (take len s)) : (quotedReversed stringState) })
    });
  scanToken

exitQuotedString :: AlexAction Token
exitQuotedString _ _ = do
    userState <- alexGetUserState;
    alexSetUserState (userState { quotedString = Nothing });
    alexSetStartCode 0;
    let stringState = fromJust (quotedString userState) in
      return (TokenString (reverse (quotedReversed stringState)))

regularToken :: (String -> Token) -> AlexAction Token
regularToken f =  \(_,_,_,s) len -> return (f (take len s))

alexEOF = do 
  userState <- alexGetUserState;
  case (delimitedString userState, quotedString userState) of
    (Just _, _) -> alexError "unterminated string"
    (_, Just _) -> alexError "unterminated string"
    (Nothing, Nothing) -> return TokenEOF


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