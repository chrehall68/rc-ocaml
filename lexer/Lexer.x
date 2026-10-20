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
    -- keywords
    and                                 { regularToken (const TokenAnd) }
    as                                  { regularToken (const TokenAs) }
    assert                              { regularToken (const TokenAssert) }
    asr                                 { regularToken (const TokenAsr) }
    begin                               { regularToken (const TokenBegin) }
    constraint                          { regularToken (const TokenConstraint) }
    do                                  { regularToken (const TokenDo) }
    done                                { regularToken (const TokenDone) }
    downto                              { regularToken (const TokenDownto) }
    else                                { regularToken (const TokenElse) }
    exception                           { regularToken (const TokenException) }
    external                            { regularToken (const TokenExternal) }
    false                               { regularToken (const TokenFalse) }
    for                                 { regularToken (const TokenFor) }
    fun                                 { regularToken (const TokenFun) }
    functor                             { regularToken (const TokenFunctor) }
    if                                  { regularToken (const TokenIf) }
    in                                  { regularToken (const TokenIn) }
    include                             { regularToken (const TokenInclude) }
    inherit                             { regularToken (const TokenInherit) }
    land                                { regularToken (const TokenLand) }
    lazy                                { regularToken (const TokenLazy) }
    let                                 { regularToken (const TokenLet) }
    lor                                 { regularToken (const TokenLor) }
    lsl                                 { regularToken (const TokenLsl) }
    lxor                                { regularToken (const TokenLxor) }
    match                               { regularToken (const TokenMatch) }
    method                              { regularToken (const TokenMethod) }
    mod                                 { regularToken (const TokenMod) }
    module                              { regularToken (const TokenModule) }
    new                                 { regularToken (const TokenNew) }
    nonrec                              { regularToken (const TokenNonrec) }
    object                              { regularToken (const TokenObject) }
    of                                  { regularToken (const TokenOf) }
    open                                { regularToken (const TokenOpen) }
    private                             { regularToken (const TokenPrivate) }
    rec                                 { regularToken (const TokenRec) }
    sig                                 { regularToken (const TokenSig) }
    struct                              { regularToken (const TokenStruct) }
    then                                { regularToken (const TokenThen) }
    true                                { regularToken (const TokenTrue) }
    try                                 { regularToken (const TokenTry) }
    type                                { regularToken (const TokenType) }
    val                                 { regularToken (const TokenVal) }
    virtual                             { regularToken (const TokenVirtual) }
    while                               { regularToken (const TokenWhile) }
    with                                { regularToken (const TokenWith) }
    -- wildcard (must precede identifiers so it wins the tie)
    \_                                  { regularToken (const TokenUnderscore) }
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
    -- operators and symbols
    \$                                  { regularToken (const TokenDollar) }
    &                                   { regularToken (const TokenAmpersand) }
    \*                                  { regularToken (const TokenStar) }
    \+                                  { regularToken (const TokenPlus) }
    \-                                  { regularToken (const TokenMinus) }
    \/                                  { regularToken (const TokenSlash) }
    =                                   { regularToken (const TokenEq) }
    \>                                  { regularToken (const TokenGt) }
    @                                   { regularToken (const TokenAt) }
    \^                                  { regularToken (const TokenCaret) }
    \|                                  { regularToken (const TokenPipe) }
    \%                                  { regularToken (const TokenPercent) }
    \<                                  { regularToken (const TokenLt) }
    \~                                  { regularToken (const TokenTilde) }
    \!                                  { regularToken (const TokenExclamation) }
    \?                                  { regularToken (const TokenQuestion) }
    \:                                  { regularToken (const TokenColon) }
    \.                                  { regularToken (const TokenDot) }
    \#                                  { regularToken (const TokenHash) }
    \!=                                 { regularToken (const TokenPhysicalNE) }
    &&                                  { regularToken (const TokenAmpersandAmpersand) }
    \'                                  { regularToken (const TokenQuote) }
    \(                                  { regularToken (const TokenLParen) }
    \)                                  { regularToken (const TokenRParen) }
    \,                                  { regularToken (const TokenComma) }
    \-\.                                { regularToken (const TokenMinusDot) }
    \-\>                                { regularToken (const TokenArrow) }
    \.\.                                { regularToken (const TokenDotDot) }
    \.\~                                { regularToken (const TokenDotTilde) }
    \:\:                                { regularToken (const TokenColonColon) }
    \:=                                 { regularToken (const TokenColonEq) }
    \:\>                                { regularToken (const TokenColonGt) }
    \;                                  { regularToken (const TokenSemi) }
    \;\;                                { regularToken (const TokenSemiSemi) }
    \<\-                                { regularToken (const TokenLeftArrow) }
    \>\]                                { regularToken (const TokenGtRBracket) }
    \>\}                                { regularToken (const TokenGtRBrace) }
    \[                                  { regularToken (const TokenLBracket) }
    \[\<                                { regularToken (const TokenLBracketLt) }
    \[\>                                { regularToken (const TokenLBracketGt) }
    \[\|                                { regularToken (const TokenLBracketPipe) }
    \]                                  { regularToken (const TokenRBracket) }
    \`                                  { regularToken (const TokenBacktick) }
    \{                                  { regularToken (const TokenLBrace) }
    \{\<                                { regularToken (const TokenLBraceLt) }
    \|\]                                { regularToken (const TokenPipeRBracket) }
    \|\|                                { regularToken (const TokenPipePipe) }
    \}                                  { regularToken (const TokenRBrace) }
  }
  -- the rest of the machinery for recognizing strings
  <delimited> {
    \| [$lower \_]* \}  /{ isEndOfDelimitedString }   { exitDelimitedString }
    [$white .]                                        { nextDelimitedStringChar }
  }
  <quoted> {
    \"                                                          { exitQuotedString }
    -- TODO - handle unicode characters?
    (@escape | $white | [^ \\])                                 { nextQuotedStringChar }
    \\ \n $white*                                               ;  -- ignore whitespace following backslash and newline
  }

{
-- our actual monad
data DelimitedStringState = DelimitedStringState
  { delimitedReversed :: String
  , delimiter         :: String
  }

data QuotedStringState = QuotedStringState
  { quotedReversed :: String
  }

data AlexUserState = AlexUserState
  { delimitedString :: Maybe DelimitedStringState
  , quotedString    :: Maybe QuotedStringState
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState Nothing Nothing

-- delimited strings
enterDelimitedString :: AlexAction Token
enterDelimitedString (_, _, _, s) len = do
  userState <- alexGetUserState
  let delim = tail (take (len - 1) s)
      state = Just (DelimitedStringState { delimitedReversed = "", delimiter = delim })
  alexSetUserState (userState { delimitedString = state })
  alexSetStartCode delimited
  scanToken

isEndOfDelimitedString :: AlexUserState -> AlexInput -> Int -> AlexInput -> Bool
isEndOfDelimitedString userState (_, _, _, s) len _ =
  tail (take (len - 1) s) == delimiter (fromJust (delimitedString userState))

-- since we take one character at a time, we can just concatenate
nextDelimitedStringChar :: AlexAction Token
nextDelimitedStringChar (_, _, _, s) _ = do
  userState <- alexGetUserState
  let stringState = fromJust (delimitedString userState)
  alexSetUserState (userState {
    delimitedString = Just (stringState { delimitedReversed = head s : delimitedReversed stringState })
  })
  scanToken

exitDelimitedString :: AlexAction Token
exitDelimitedString _ _ = do
  userState <- alexGetUserState
  let stringState = fromJust (delimitedString userState)
  alexSetUserState (userState { delimitedString = Nothing })
  alexSetStartCode 0
  return (TokenString (reverse (delimitedReversed stringState)))

-- quoted strings
enterQuotedString :: AlexAction Token
enterQuotedString _ _ = do
  userState <- alexGetUserState
  let state = Just (QuotedStringState { quotedReversed = "" })
  alexSetUserState (userState { quotedString = state })
  alexSetStartCode quoted
  scanToken

nextQuotedStringChar :: AlexAction Token
nextQuotedStringChar (_, _, _, s) len = do
  userState <- alexGetUserState
  let stringState = fromJust (quotedString userState)
  alexSetUserState (userState {
    quotedString = Just (stringState { quotedReversed = decodeChar (take len s) : quotedReversed stringState })
  })
  scanToken

exitQuotedString :: AlexAction Token
exitQuotedString _ _ = do
  userState <- alexGetUserState
  let stringState = fromJust (quotedString userState)
  alexSetUserState (userState { quotedString = Nothing })
  alexSetStartCode 0
  return (TokenString (reverse (quotedReversed stringState)))

regularToken :: (String -> Token) -> AlexAction Token
regularToken f = \(_, _, _, s) len -> return (f (take len s))

alexEOF :: Alex Token
alexEOF = do
  userState <- alexGetUserState
  case (delimitedString userState, quotedString userState) of
    (Just _, _)        -> alexError "unterminated string"
    (_, Just _)        -> alexError "unterminated string"
    (Nothing, Nothing) -> return TokenEOF

-- similar to alexMonadScan, but calls alexScanUser instead of just alexScan
scanToken :: Alex Token
scanToken = do
  inp <- alexGetInput
  sc  <- alexGetStartCode
  us  <- alexGetUserState
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
alexScanTokens source = runAlex source loop
  where
    loop = do
      tok <- scanToken
      case tok of
        TokenEOF -> return []
        _        -> (tok :) <$> loop
}
