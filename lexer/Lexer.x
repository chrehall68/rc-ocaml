{
module Lexer (alexScanTokens)  where
import Tokens
import Data.Array
import Data.Char
import Data.Maybe
import Control.Monad
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

-- identifiers
$letter = [$lower $upper]
$identifierSuffix = [$letter $digit \_ \']
$ident = [$letter \_]
@lowercaseIdent = [$lower \_] $identifierSuffix*
@capitalizedIdent = $upper $identifierSuffix*

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
-- prefix and infix symbols
-- supposed to be lexed as a single token according to https://ocaml.org/manual/5.3/lex.html#sss:lex-ops-symbols
$coreOperatorChar = [\$ & \* \+ \- \/ = \> @ \^ \| ]
$operatorChar = [$coreOperatorChar \~ \! \? \% \< \: \. ]
@infixSymbol = ([$coreOperatorChar \% \<] [$operatorChar]*) | (\# [$operatorChar]+)
@prefixSymbol = (\! $operatorChar*) | ([\? \~] [$operatorChar]+)

tokens :-
  <0> {
    $white+                             ;
    -- keywords
    and                                 { regularToken (const TokenAnd) }
    as                                  { regularToken (const TokenAs) }
    assert                              { regularToken (const TokenAssert) }
    asr                                 { regularToken (const TokenAsr) }
    begin                               { regularToken (const TokenBegin) }
    class                               { regularToken (const TokenClass) }
    constraint                          { regularToken (const TokenConstraint) }
    do                                  { regularToken (const TokenDo) }
    done                                { regularToken (const TokenDone) }
    downto                              { regularToken (const TokenDownto) }
    else                                { regularToken (const TokenElse) }
    end                                 { regularToken (const TokenEnd) }
    exception                           { regularToken (const TokenException) }
    external                            { regularToken (const TokenExternal) }
    false                               { regularToken (const TokenFalse) }
    for                                 { regularToken (const TokenFor) }
    fun                                 { regularToken (const TokenFun) }
    function                            { regularToken (const TokenFunction) }
    functor                             { regularToken (const TokenFunctor) }
    if                                  { regularToken (const TokenIf) }
    in                                  { regularToken (const TokenIn) }
    include                             { regularToken (const TokenInclude) }
    inherit                             { regularToken (const TokenInherit) }
    initializer                         { regularToken (const TokenInitializer) }
    land                                { regularToken (const TokenLand) }
    lazy                                { regularToken (const TokenLazy) }
    let                                 { regularToken (const TokenLet) }
    lor                                 { regularToken (const TokenLor) }
    lsl                                 { regularToken (const TokenLsl) }
    lsr                                 { regularToken (const TokenLsr) }
    lxor                                { regularToken (const TokenLxor) }
    match                               { regularToken (const TokenMatch) }
    method                              { regularToken (const TokenMethod) }
    mod                                 { regularToken (const TokenMod) }
    module                              { regularToken (const TokenModule) }
    mutable                             { regularToken (const TokenMutable) }
    new                                 { regularToken (const TokenNew) }
    nonrec                              { regularToken (const TokenNonrec) }
    object                              { regularToken (const TokenObject) }
    of                                  { regularToken (const TokenOf) }
    open                                { regularToken (const TokenOpen) }
    or                                  { regularToken (const TokenOr) }
    private                             { regularToken (const TokenPrivate) }
    rec                                 { regularToken (const TokenRec) }
    sig                                 { regularToken (const TokenSig) }
    struct                              { regularToken (const TokenStruct) }
    then                                { regularToken (const TokenThen) }
    to                                  { regularToken (const TokenTo) }
    true                                { regularToken (const TokenTrue) }
    try                                 { regularToken (const TokenTry) }
    type                                { regularToken (const TokenType) }
    val                                 { regularToken (const TokenVal) }
    virtual                             { regularToken (const TokenVirtual) }
    when                                { regularToken (const TokenWhen) }
    while                               { regularToken (const TokenWhile) }
    with                                { regularToken (const TokenWith) }
    -- keyword symbols
    \!=                                 { regularToken (const TokenPhysicalNE) }
    \#                                  { regularToken (const TokenHash) }
    &                                   { regularToken (const TokenAmpersand) }
    &&                                  { regularToken (const TokenAmpersandAmpersand) }
    \'                                  { regularToken (const TokenQuote) }
    \(                                  { regularToken (const TokenLParen) }
    \)                                  { regularToken (const TokenRParen) }
    \*                                  { regularToken (const TokenStar) }
    \+                                  { regularToken (const TokenPlus) }
    \,                                  { regularToken (const TokenComma) }
    \-                                  { regularToken (const TokenMinus) }
    \-\.                                { regularToken (const TokenMinusDot) }
    \-\>                                { regularToken (const TokenArrow) }
    \.                                  { regularToken (const TokenDot) }
    \.\.                                { regularToken (const TokenDotDot) }
    \.\~                                { regularToken (const TokenDotTilde) }
    \:                                  { regularToken (const TokenColon) }
    \:\:                                { regularToken (const TokenColonColon) }
    \:=                                 { regularToken (const TokenColonEq) }
    \:\>                                { regularToken (const TokenColonGt) }
    \;                                  { regularToken (const TokenSemi) }
    \;\;                                { regularToken (const TokenSemiSemi) }
    \<                                  { regularToken (const TokenLt) }
    \<\-                                { regularToken (const TokenLeftArrow) }
    =                                   { regularToken (const TokenEq) }
    \>                                  { regularToken (const TokenGt) }
    \>\]                                { regularToken (const TokenGtRBracket) }
    \>\}                                { regularToken (const TokenGtRBrace) }
    \?                                  { regularToken (const TokenQuestion) }
    \[                                  { regularToken (const TokenLBracket) }
    \[\<                                { regularToken (const TokenLBracketLt) }
    \[\>                                { regularToken (const TokenLBracketGt) }
    \[\|                                { regularToken (const TokenLBracketPipe) }
    \]                                  { regularToken (const TokenRBracket) }
    \_                                  { regularToken (const TokenUnderscore) }
    \`                                  { regularToken (const TokenBacktick) }
    \{                                  { regularToken (const TokenLBrace) }
    \{\<                                { regularToken (const TokenLBraceLt) }
    \|                                  { regularToken (const TokenPipe) }
    \|\]                                { regularToken (const TokenPipeRBracket) }
    \|\|                                { regularToken (const TokenPipePipe) }
    \}                                  { regularToken (const TokenRBrace) }
    \~                                  { regularToken (const TokenTilde) }
    \(\*                                { enterComment }
    -- identifiers
    @capitalizedIdent                   { regularToken (\s -> TokenCapitalizedIdentifier s) }
    @lowercaseIdent                     { regularToken (\s -> TokenLowercaseIdentifier s) }
    -- integers
    @int32Literal                       { regularToken (\s -> TokenInt (IntLiteral {value=(decodeOcamlInt . init $ s), representation=Int32}) ) }
    @int64Literal                       { regularToken (\s -> TokenInt (IntLiteral {value=(decodeOcamlInt . init $ s), representation=Int64}) ) }
    @nativeIntLiteral                   { regularToken (\s -> TokenInt (IntLiteral {value=(decodeOcamlInt . init $ s), representation=NativeInt}) ) }
    @integerLiteral                     { regularToken (\s -> TokenInt (IntLiteral {value=(decodeOcamlInt s), representation=Unspecified}) ) }
    -- floats
    @floatBase10                        { regularToken (\s -> TokenDouble (decodeOcamlDouble s) )}
    @floatBase16                        { regularToken (\s -> TokenDouble (decodeOcamlDouble s) )}
    -- characters
    \' (@regularChar | @escape) \'      { regularToken (\s -> TokenChar (decodeChar (tail . init $ s))) }
    -- strings
    \"                                  { enterQuotedString }
    \{ [$lower \_]* \|                  { enterDelimitedString }
    -- naming labels
    -- see https://ocaml.org/manual/5.3/lex.html#sss:labelname
    \~ @lowercaseIdent \:               { regularToken (\s -> TokenLabel (tail . init $ s)) }
    \? @lowercaseIdent \:               { regularToken (\s -> TokenOptionalLabel (tail . init $ s)) }
    -- prefix/infix operators
    @infixSymbol                        { regularToken (\s -> TokenInfix s) }
    @prefixSymbol                       { regularToken (\s -> TokenPrefix s) }
  }
  -- the rest of the machinery for recognizing strings
  <delimited> {
    \| [$lower \_]* \}  /{ isEndOfDelimitedString }   { exitDelimitedString }
    [$white .]                                        { nextDelimitedStringChar }
  }
  <quoted> {
    \"                                                          { exitQuotedString }
    \\ u \{ [$hexDigit]+ \}                                     { nextQuotedStringUnicodeChar }
    (@escape | $white | [^ \\])                                 { nextQuotedStringChar }
    \\ \n $white*                                               ;  -- ignore whitespace following backslash and newline
  }
  <comment> {
    \(\*                                { openNestedComment }
    \*\)                                { closeNestedComment }
    [$white .]                          ;
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

data CommentState = CommentState
  { commentDepth :: Integer
  }

data AlexUserState = AlexUserState
  { delimitedString :: Maybe DelimitedStringState
  , quotedString    :: Maybe QuotedStringState
  , commentActive   :: Maybe CommentState
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState Nothing Nothing Nothing

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

nextQuotedStringUnicodeChar :: AlexAction Token
nextQuotedStringUnicodeChar (_, _, _, s) len = do
  userState <- alexGetUserState
  let stringState = fromJust (quotedString userState)
  alexSetUserState (userState {
    quotedString = Just (stringState { quotedReversed = decodeUnicodeChar (take len s) : quotedReversed stringState })
  })
  scanToken

exitQuotedString :: AlexAction Token
exitQuotedString _ _ = do
  userState <- alexGetUserState
  let stringState = fromJust (quotedString userState)
  alexSetUserState (userState { quotedString = Nothing })
  alexSetStartCode 0
  return (TokenString (reverse (quotedReversed stringState)))

-- comments
enterComment :: AlexAction Token
enterComment _ _ = do 
  userState <- alexGetUserState
  let state = Just (CommentState { commentDepth = 1 })
  alexSetUserState (userState { commentActive = state })
  alexSetStartCode comment
  scanToken

openNestedComment :: AlexAction Token
openNestedComment _ _ = do
  userState <- alexGetUserState
  let commentState = fromJust (commentActive userState)
  alexSetUserState (userState {
    commentActive = Just (commentState { commentDepth = 1 + (commentDepth commentState) })
  })
  scanToken

closeNestedComment :: AlexAction Token
closeNestedComment _ _ = do
  userState <- alexGetUserState
  let commentState = fromJust (commentActive userState)
  alexSetUserState (userState {
    commentActive = Just (commentState { commentDepth = (commentDepth commentState) - 1 })
  })
  -- This code automatically switches the parser state back to normal
  when ((commentDepth commentState) == 1) $ do
    alexSetUserState (userState { commentActive = Nothing })
    alexSetStartCode 0
  scanToken

regularToken :: (String -> Token) -> AlexAction Token
regularToken f = \(_, _, _, s) len -> return (f (take len s))

alexEOF :: Alex Token
alexEOF = do
  userState <- alexGetUserState
  case (delimitedString userState, quotedString userState, commentActive userState) of
    (Just _, _, _)        -> alexError $ "unterminated string literal; did not find matching |" ++ delimiter (fromJust (delimitedString userState)) ++ "}"
    (_, Just _, _)        -> alexError "unterminated string literal; no matching closing quote found"
    (_, _, Just _)        -> alexError "unterminated comment; no matching closing comment found"
    (Nothing, Nothing, Nothing) -> return TokenEOF

-- similar to alexMonadScan, but calls alexScanUser instead of just alexScan
scanToken :: Alex Token
scanToken = do
  inp <- alexGetInput
  sc  <- alexGetStartCode
  us  <- alexGetUserState
  case alexScanUser us inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line col), _, _, _) ->
      alexError ("Unexpected token at line " ++ show line ++ ", column " ++ show col)
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
