{
module MyParser where
-- Import any AST or token types needed here
import Ast
import Tokens
import Data.Char
}

%name calc
%tokentype { Token }
%error { parseError }

%token
      -- literals and identifiers
      INT               { TokenInt $$ }
      DOUBLE            { TokenDouble $$ }
      CHAR              { TokenChar $$ }
      STRING            { TokenString $$ }
      LABEL             { TokenLabel $$ }
      OPTLABEL          { TokenOptionalLabel $$ }
      CAPITALIDENT      { TokenCapitalizedIdentifier $$ }
      LOWERIDENT        { TokenLowercaseIdentifier $$ }
      INFIX             { TokenInfix $$ }
      PREFIX            { TokenPrefix $$ }

      -- keywords
      AND               { TokenAnd }
      AS                { TokenAs }
      ASSERT            { TokenAssert }
      ASR               { TokenAsr }
      BEGIN             { TokenBegin }
      CONSTRAINT        { TokenConstraint }
      DO                { TokenDo }
      DONE              { TokenDone }
      DOWNTO            { TokenDownto }
      ELSE              { TokenElse }
      EXCEPTION         { TokenException }
      EXTERNAL          { TokenExternal }
      FALSE             { TokenFalse }
      FOR               { TokenFor }
      FUN               { TokenFun }
      FUNCTOR           { TokenFunctor }
      IF                { TokenIf }
      IN                { TokenIn }
      INCLUDE           { TokenInclude }
      INHERIT           { TokenInherit }
      LAND              { TokenLand }
      LAZY              { TokenLazy }
      LET               { TokenLet }
      LOR               { TokenLor }
      LSL               { TokenLsl }
      LXOR              { TokenLxor }
      MATCH             { TokenMatch }
      METHOD            { TokenMethod }
      MOD               { TokenMod }
      MODULE            { TokenModule }
      NEW               { TokenNew }
      NONREC            { TokenNonrec }
      OBJECT            { TokenObject }
      OF                { TokenOf }
      OPEN              { TokenOpen }
      PRIVATE           { TokenPrivate }
      REC               { TokenRec }
      SIG               { TokenSig }
      STRUCT            { TokenStruct }
      THEN              { TokenThen }
      TRUE              { TokenTrue }
      TRY               { TokenTry }
      TYPE              { TokenType }
      VAL               { TokenVal }
      VIRTUAL           { TokenVirtual }
      WHILE             { TokenWhile }
      WITH              { TokenWith }

      -- keyword symbols
      '!='              { TokenPhysicalNE }
      '#'               { TokenHash }
      '&'               { TokenAmpersand }
      '&&'              { TokenAmpersandAmpersand }
      QUOTE             { TokenQuote }
      '('               { TokenLParen }
      ')'               { TokenRParen }
      '*'               { TokenStar }
      '+'               { TokenPlus }
      ','               { TokenComma }
      '-'               { TokenMinus }
      '-.'              { TokenMinusDot }
      '->'              { TokenArrow }
      '.'               { TokenDot }
      '..'              { TokenDotDot }
      '.~'              { TokenDotTilde }
      ':'               { TokenColon }
      '::'              { TokenColonColon }
      ':='              { TokenColonEq }
      ':>'              { TokenColonGt }
      ';'               { TokenSemi }
      ';;'              { TokenSemiSemi }
      '<'               { TokenLt }
      '<-'              { TokenLeftArrow }
      '='               { TokenEq }
      '>'               { TokenGt }
      '>]'              { TokenGtRBracket }
      '>}'              { TokenGtRBrace }
      '?'               { TokenQuestion }
      '['               { TokenLBracket }
      '[<'              { TokenLBracketLt }
      '[>'              { TokenLBracketGt }
      '[|'              { TokenLBracketPipe }
      ']'               { TokenRBracket }
      '_'               { TokenUnderscore }
      BACKTICK          { TokenBacktick }
      '{'               { TokenLBrace }
      '{<'              { TokenLBraceLt }
      '|'               { TokenPipe }
      '|]'              { TokenPipeRBracket }
      '||'              { TokenPipePipe }
      '}'               { TokenRBrace }
      '~'               { TokenTilde }

      EOF               { TokenEOF }

%%

-- Define your BNF grammar rules here

-- Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
--       | Exp1                    { Exp1 $1 }

-- Exp1  : Exp1 '+' Term           { Plus $1 $3 }
--       | Exp1 '-' Term           { Minus $1 $3 }
--       | Term                    { Term $1 }

-- Term  : Term '*' Factor         { Times $1 $3 }
--       | Term '/' Factor         { Div $1 $3 }
--       | Factor                  { Factor $1 }

-- Factor
--       : int                     { Int $1 }
--       | var                     { Var $1 }
--       | '(' Exp ')'             { Brack $2 }
Factor
  : INT  {AstInt $1}

{
-- TODO - recover from errors
-- see https://haskell-happy.readthedocs.io/en/latest/using.html#handling-parse-errors
parseError :: [Token] -> a
parseError _ = error "Parse error"

}