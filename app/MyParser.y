{
module MyParser where
-- Import any AST or token types needed here
import Ast
import Tokens
import Data.Char
}

%monad { ParseResult } { (>>=) } { return }
%name parse Program
%tokentype { Token }
%error { parseError }

%token
      -- literals and identifiers
      INT         { TokenInt $$ }
      DOUBLE      { TokenDouble $$ }
      CHAR        { TokenChar $$ }
      STRING      { TokenString $$ }
      LABEL       { TokenLabel $$ }
      OPTLABEL    { TokenOptionalLabel $$ }
      UIDENT      { TokenCapitalizedIdentifier $$ }
      LIDENT      { TokenLowercaseIdentifier $$ }
      INFIX       { TokenInfix $$ }
      PREFIX      { TokenPrefix $$ }

      -- keywords
      AND         { TokenAnd }
      AS          { TokenAs }
      ASR         { TokenAsr }
      ASSERT      { TokenAssert }
      BEGIN       { TokenBegin }
      CLASS       { TokenClass }
      CONSTRAINT  { TokenConstraint }
      DO          { TokenDo }
      DONE        { TokenDone }
      DOWNTO      { TokenDownto }
      ELSE        { TokenElse }
      END         { TokenEnd }
      EXCEPTION   { TokenException }
      EXTERNAL    { TokenExternal }
      FALSE       { TokenFalse }
      FOR         { TokenFor }
      FUN         { TokenFun }
      FUNCTION    { TokenFunction }
      FUNCTOR     { TokenFunctor }
      IF          { TokenIf }
      IN          { TokenIn }
      INCLUDE     { TokenInclude }
      INHERIT     { TokenInherit }
      INITIALIZER { TokenInitializer }
      LAND        { TokenLand }
      LAZY        { TokenLazy }
      LET         { TokenLet }
      LOR         { TokenLor }
      LSL         { TokenLsl }
      LSR         { TokenLsr }
      LXOR        { TokenLxor }
      MATCH       { TokenMatch }
      METHOD      { TokenMethod }
      MOD         { TokenMod }
      MODULE      { TokenModule }
      MUTABLE     { TokenMutable }
      NEW         { TokenNew }
      NONREC      { TokenNonrec }
      OBJECT      { TokenObject }
      OF          { TokenOf }
      OPEN        { TokenOpen }
      OR          { TokenOr }
      PRIVATE     { TokenPrivate }
      REC         { TokenRec }
      SIG         { TokenSig }
      STRUCT      { TokenStruct }
      THEN        { TokenThen }
      TO          { TokenTo }
      TRUE        { TokenTrue }
      TRY         { TokenTry }
      TYPE        { TokenType }
      VAL         { TokenVal }
      VIRTUAL     { TokenVirtual }
      WHEN        { TokenWhen }
      WHILE       { TokenWhile }
      WITH        { TokenWith }

      -- keyword symbols
      '!='        { TokenPhysicalNE }
      '#'         { TokenHash }
      '&'         { TokenAmpersand }
      '&&'        { TokenAmpersandAmpersand }
      QUOTE       { TokenQuote } -- single quote
      '('         { TokenLParen }
      ')'         { TokenRParen }
      '*'         { TokenStar }
      '+'         { TokenPlus }
      ','         { TokenComma }
      '-'         { TokenMinus }
      '-.'        { TokenMinusDot }
      '->'        { TokenArrow }
      '.'         { TokenDot }
      '..'        { TokenDotDot }
      '.~'        { TokenDotTilde }
      ':'         { TokenColon }
      '::'        { TokenColonColon }
      ':='        { TokenColonEq }
      ':>'        { TokenColonGt }
      ';'         { TokenSemi }
      ';;'        { TokenSemiSemi }
      '<'         { TokenLt }
      '<-'        { TokenLeftArrow }
      '='         { TokenEq }
      '>'         { TokenGt }
      '>]'        { TokenGtRBracket }
      '>}'        { TokenGtRBrace }
      '?'         { TokenQuestion }
      '['         { TokenLBracket }
      '[<'        { TokenLBracketLt }
      '[>'        { TokenLBracketGt }
      '[|'        { TokenLBracketPipe }
      ']'         { TokenRBracket }
      '_'         { TokenUnderscore }
      BACKTICK    { TokenBacktick }
      '{'         { TokenLBrace }
      '{<'        { TokenLBraceLt }
      '|'         { TokenPipe }
      '|]'        { TokenPipeRBracket }
      '||'        { TokenPipePipe }
      '}'         { TokenRBrace }
      '~'         { TokenTilde }

      EOF         { TokenEOF }

%%

-- Define your BNF grammar rules here

-- Macros for faster development
-- allows equivalent of p?
opt(p)
  : p          { Just $1 }
  | {- empty -} { Nothing }

-- helper for `many`
many_rev(p)
  :                { [] }
  | many_rev(p) p  { $2 : $1 }
-- allows equivalent of p*
many(p)  : many_rev(p)  { reverse $1 }

-- helper for `many1`
many1_rev(p)
  : p              { [$1] }
  | many1_rev(p) p { $2 : $1 }
-- allows equivalent of p+
many1(p) : many1_rev(p) { reverse $1 }

-- 11.3 Names (https://ocaml.org/manual/5.3/names.html)

-- naming objects
ValueName
  : LIDENT                      { $1 }
  | '(' OperatorName ')'        { $2 }

OperatorName
  : PREFIX                      { $1 }
  | InfixOp                     { $1 }

InfixOp
  : INFIX                       { $1 }
  | '*'                         { "*" }
  | '+'                         { "+" }
  | '-'                         { "-" }
  | '-.'                        { "-." }
  | '='                         { "=" }
  | '!='                        { "!=" }
  | '<'                         { "<" }
  | '>'                         { ">" }
  | OR                          { "or" }
  | '||'                        { "||" }
  | '&'                         { "&" }
  | '&&'                        { "&&" }
  | ':='                        { ":=" }
  | MOD                         { "mod" }
  | LAND                        { "land" }
  | LOR                         { "lor" }
  | LXOR                        { "lxor" }
  | LSL                         { "lsl" }
  | LSR                         { "lsr" }
  | ASR                         { "asr" }

ConstrName      : UIDENT        { $1 }
TagName         : UIDENT        { $1 }
TypeconstrName  : LIDENT        { $1 }
FieldName       : LIDENT        { $1 }
ModuleName      : UIDENT        { $1 }
ModtypeName     : UIDENT        { $1 }
                | LIDENT        { $1 }
ClassName       : LIDENT        { $1 }
InstVarName     : LIDENT        { $1 }
MethodName      : LIDENT        { $1 }

-- referring to named objects
ValuePath
  : ValueName                   { Path Nothing $1 }
  | ModulePath '.' ValueName    { Path (Just $1) $3 }

Constr
  : ConstrName                  { Path Nothing $1 }
  | ModulePath '.' ConstrName   { Path (Just $1) $3 }

Typeconstr
  : TypeconstrName                          { ExtendedPath Nothing $1 }
  | ExtendedModulePath '.' TypeconstrName   { ExtendedPath (Just $1) $3 }

Field
  : FieldName                   { Path Nothing $1 }
  | ModulePath '.' FieldName    { Path (Just $1) $3 }

ModtypePath
  : ModtypeName                             { ExtendedPath Nothing $1 }
  | ExtendedModulePath '.' ModtypeName      { ExtendedPath (Just $1) $3 }

ClassPath
  : ClassName                   { Path Nothing $1 }
  | ModulePath '.' ClassName    { Path (Just $1) $3 }

ClasstypePath
  : ClassName                               { ExtendedPath Nothing $1 }
  | ExtendedModulePath '.' ClassName        { ExtendedPath (Just $1) $3 }

ModulePath
  : ModuleName                  { ModulePath [$1] }
  | ModulePath '.' ModuleName   { appendModule $1 $3 }

ExtendedModulePath
  : ExtendedModuleName                              { ExtendedModulePath [$1] }
  | ExtendedModulePath '.' ExtendedModuleName       { appendExtendedModule $1 $3 }

ExtendedModuleName
  : ModuleName                                      { ExtendedModuleName $1 [] }
  | ExtendedModuleName '(' ExtendedModulePath ')'   { applyFunctor $1 $3 }

-- 11.5 Constants (https://ocaml.org/manual/5.3/const.html)
-- TODO - add more
Constant 
  : INT                  { IntConstant $1 }
  | DOUBLE               { DoubleConstant $1 }
  | CHAR                 { CharConstant $1 }
  | STRING               { StringConstant $1 }

-- 11.7 Expressions (https://ocaml.org/manual/5.3/expr.html)
Expr
  : Constant                  { Expr $1 }

-- 11.10 Module Types (https://ocaml.org/manual/5.3/modtypes.html)
-- TODO: fill out this section
-- Specification :                               { Specification }

-- 11.11 Module Expressions (https://ocaml.org/manual/5.3/modules.html)
-- The manual writes this as
--   { ;; } ( definition | expr ) { { ;; } definition | ;; expr } { ;; }
-- but written out literally it is ambiguous: in `;; ;; expr` the parser
-- can't tell which `;;` belongs to the `{ ;; }` run and which to `;; expr`.
-- so, instead, we give every `;;` exactly one role:
-- either it introduces an expr, or it is a stray separator.
ModuleItems
  : many(';;') DefinitionOrExpr many(ModuleItem)  { ModuleItems ($2 : concat $3) }

ModuleItem
  : Definition        { [$1] }
  | ';;' Expr         { [$2] }
  | ';;'              { [] }

DefinitionOrExpr 
  : Definition        { $1 }
  | Expr              { $1 }

-- TODO: use actual definitions
Definition 
  : LET                  { Definition }


-- 11.12 Compilation Units (https://ocaml.org/manual/5.3/compunit.html)

Program
  : UnitInterface UnitImplementation           { Program $1 $2 }

-- UnitInterface : many(Specification opt(';;'))             { $1 }
UnitInterface :                               { UnitInterface }
 
UnitImplementation : opt(ModuleItems)         { UnitImplementation $1 }


{

type ParseResult a = Either String a
-- TODO - recover from errors
-- see https://haskell-happy.readthedocs.io/en/latest/using.html#handling-parse-errors
parseError :: [Token] -> ParseResult a
parseError t = Left ("Parse error on " ++ (show t))

appendModule :: ModulePath -> String -> ModulePath
appendModule (ModulePath ms) m = ModulePath (ms ++ [m])

appendExtendedModule :: ExtendedModulePath -> ExtendedModuleName -> ExtendedModulePath
appendExtendedModule (ExtendedModulePath ms) m = ExtendedModulePath (ms ++ [m])

applyFunctor :: ExtendedModuleName -> ExtendedModulePath -> ExtendedModuleName
applyFunctor (ExtendedModuleName m args) arg = ExtendedModuleName m (args ++ [arg])

}